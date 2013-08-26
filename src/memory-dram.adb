
package body Memory.DRAM is

   function Create_DRAM(cas_cycles     : Time_Type;
                        rcd_cycles     : Time_Type;
                        rp_cycles      : Time_Type;
                        word_size      : Positive;
                        page_words     : Positive;
                        row_count      : Positive;
                        open_page_mode : Boolean) return DRAM_Pointer is
      result : constant DRAM_Pointer := new DRAM_Type;
   begin
      result.bank_size        := word_size * page_words * row_count;
      result.page_size        := word_size * page_words;
      result.cas_cycles       := cas_cycles;
      result.rcd_cycles       := rcd_cycles;
      result.rp_cycles        := rp_cycles;
      result.word_size        := word_size;
      result.page_words       := page_words;
      result.row_count        := row_count;
      result.open_page_mode   := open_page_mode;
      return result;
   end Create_DRAM;

   function Clone(mem : DRAM_Type) return Memory_Pointer is
      result : constant DRAM_Pointer := new DRAM_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Reset(mem     : in out DRAM_Type;
                   context : in Natural) is
   begin
      mem.banks.Clear;
      mem.writes := 0;
   end Reset;

   function Get_Bank(mem      : DRAM_Type;
                     address  : Address_Type) return Natural is
   begin
      return Natural(address / Address_Type(mem.bank_size));
   end Get_Bank;

   function Get_Page(mem      : DRAM_Type;
                     address  : Address_Type) return Address_Type is
   begin
      return address / Address_Type(mem.page_size);
   end Get_Page;

   procedure Process(mem      : in out DRAM_Type;
                     address  : in Address_Type) is
      bank_index  : constant Natural := Get_Bank(mem, address);
      page_index  : constant Address_Type := Get_Page(mem, address);
      now         : constant Time_Type    := Get_Time(mem);
      cycles      : Time_Type := 0;
      extra       : Time_Type := 0;
   begin
      if bank_index > mem.banks.Last_Index then
         mem.banks.Set_Length(Count_Type(bank_index + 1));
      end if;
      declare
         bank : Bank_Type := mem.banks.Element(bank_index);
      begin

         -- Make sure this bank is ready for another request.
         if now < bank.pending then
            cycles := cycles + bank.pending - now;
         end if;

         if not mem.open_page_mode then
            -- Closed page mode.
            cycles := cycles + mem.rcd_cycles;  -- Open the page.
            cycles := cycles + mem.cas_cycles;  -- Access the column.
            extra  := extra  + mem.rp_cycles;   -- Precharge (next access).
         elsif bank.page = page_index then
            -- The correct page is open.
            cycles := cycles + mem.cas_cycles;
         else
            -- The wrong page is open.
            cycles := cycles + mem.rp_cycles;   -- Precharge.
            cycles := cycles + mem.rcd_cycles;  -- Open the page.
            cycles := cycles + mem.cas_cycles;  -- Access the column.
         end if;
         bank.pending   := now + cycles + extra;
         bank.page      := page_index;
         mem.banks.Replace_Element(bank_index, bank);
         Advance(mem, cycles);
      end;
   end Process;

   procedure Process(mem   : in out DRAM_Type;
                     start : in Address_Type;
                     size  : in Positive) is
      last  : constant Address_Type := start + Address_Type(size);
      wsize : constant Address_Type := Address_Type(mem.word_size);
      temp  : Address_Type := start;
   begin
      while temp < last loop
         Process(mem, temp);
         temp := temp - (temp mod wsize) + wsize;
      end loop;
   end Process;

   procedure Read(mem      : in out DRAM_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Process(mem, address, size);
   end Read;

   procedure Write(mem     : in out DRAM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Process(mem, address, size);
      mem.writes := mem.writes + 1;
   end Write;

   procedure Idle(mem      : in out DRAM_Type;
                  cycles   : in Time_Type) is
   begin
      Advance(mem, cycles);
   end Idle;

   function To_String(mem : DRAM_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(dram ");
      Append(result, "(cas_cycles " & To_String(mem.cas_cycles) & ")");
      Append(result, "(rcd_cycles " & To_String(mem.rcd_cycles) & ")");
      Append(result, "(rp_cycles " & To_String(mem.rp_cycles) & ")");
      Append(result, "(word_size " & To_String(mem.word_size) & ")");
      Append(result, "(page_words " & To_String(mem.page_words) & ")");
      Append(result, "(row_count " & To_String(mem.row_count) & ")");
      if mem.open_page_mode then
         Append(result, "(open_page true)");
      else
         Append(result, "(open_page false)");
      end if;
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : DRAM_Type) return Cost_Type is
   begin
      return 0;
   end Get_Cost;

   function Get_Writes(mem : DRAM_Type) return Long_Integer is
   begin
      return mem.writes;
   end Get_Writes;

   function Get_Word_Size(mem : DRAM_Type) return Positive is
   begin
      return mem.word_size;
   end Get_Word_Size;

   function Get_Ports(mem : DRAM_Type) return Port_Vector_Type is
      result   : Port_Vector_Type;
      port     : constant Port_Type := Get_Port(mem);
   begin
      result.Append(port);
      return result;
   end Get_Ports;

   procedure Generate(mem  : in DRAM_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      name        : constant String := "m" & To_String(Get_ID(mem));
      pname       : constant String := "p" & To_String(Get_ID(mem));
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
   begin
      Declare_Signals(sigs, name, word_bits);
      Declare_Signals(sigs, pname, word_bits);
      Line(code, pname & "_addr <= " & name & "_addr;");
      Line(code, pname & "_din <= " & name & "_din;");
      Line(code, name & "_dout <= " & pname & "_dout;");
      Line(code, pname & "_re <= " & name & "_re;");
      Line(code, pname & "_we <= " & name & "_we;");
      Line(code, pname & "_mask <= " & name & "_mask;");
      Line(code, name & "_ready <= " & pname & "_ready;");
   end Generate;

end Memory.DRAM;
