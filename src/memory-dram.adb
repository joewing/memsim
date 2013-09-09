
with Ada.Assertions; use Ada.Assertions;
with Device;         use Device;

package body Memory.DRAM is

   function Create_DRAM(cas_cycles     : Time_Type;
                        rcd_cycles     : Time_Type;
                        rp_cycles      : Time_Type;
                        wb_cycles      : Time_Type;
                        multiplier     : Time_Type;
                        word_size      : Positive;
                        page_size      : Positive;
                        page_count     : Positive;
                        width          : Positive;
                        burst_size     : Positive;
                        open_page_mode : Boolean) return DRAM_Pointer is
      result : DRAM_Pointer;
   begin

      -- Make sure this is a valid memory.
      if page_size mod word_size /= 0 then
         return null;
      end if;
      if word_size mod width /= 0 then
         return null;
      end if;

      result := new DRAM_Type;

      result.bank_size        := page_size * page_count;
      result.cas_cycles       := cas_cycles;
      result.rcd_cycles       := rcd_cycles;
      result.rp_cycles        := rp_cycles;
      result.wb_cycles        := wb_cycles;
      result.multiplier       := multiplier;
      result.access_cycles    := Time_Type(burst_size);
      result.word_size        := word_size;
      result.page_size        := page_size;
      result.page_count       := page_count;
      result.width            := width;
      result.burst_size       := burst_size;
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
                     address  : in Address_Type;
                     last     : in Boolean;
                     is_write : in Boolean) is
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
            -- Open, access, close.
            cycles := cycles + mem.cas_cycles * mem.multiplier;
            cycles := cycles + mem.rcd_cycles * mem.multiplier;
            cycles := cycles + mem.access_cycles * mem.multiplier;
            extra  := extra  + mem.rp_cycles * mem.multiplier;
            if is_write then
               extra := extra + mem.wb_cycles * mem.multiplier;
            end if;
         elsif bank.page = page_index then
            -- The correct page is open.
            cycles := cycles + mem.cas_cycles * mem.multiplier;
            cycles := cycles + mem.access_cycles * mem.multiplier;
            bank.dirty := bank.dirty or is_write;
         else
            -- The wrong page is open.
            cycles := cycles + mem.rp_cycles * mem.multiplier;
            cycles := cycles + mem.rcd_cycles * mem.multiplier;
            cycles := cycles + mem.cas_cycles * mem.multiplier;
            cycles := cycles + mem.access_cycles * mem.multiplier;
            if bank.dirty then
               cycles := cycles + mem.wb_cycles * mem.multiplier;
            end if;
            bank.dirty := is_write;
         end if;
         bank.pending   := now + cycles + extra;
         bank.page      := page_index;
         mem.banks.Replace_Element(bank_index, bank);
         Advance(mem, cycles);
      end;
   end Process;

   procedure Process(mem      : in out DRAM_Type;
                     start    : in Address_Type;
                     size     : in Positive;
                     is_write : in Boolean) is
      last  : constant Address_Type := start + Address_Type(size);
      bsize : constant Address_Type := Address_Type(mem.burst_size *
                                                    mem.width);
      temp  : Address_Type := start;
      next  : Address_Type;
   begin
      Assert(start < Address_Type(2) ** Get_Address_Bits,
             "invalid address in Memory.DRAM.Process");
      while temp < last loop
         next := temp - (temp mod bsize) + bsize;
         Process(mem, temp, next >= last, is_write);
         temp := next;
      end loop;
   end Process;

   procedure Read(mem      : in out DRAM_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Process(mem, address, size, False);
   end Read;

   procedure Write(mem     : in out DRAM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Process(mem, address, size, True);
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
      Append(result, "(multiplier " & To_String(mem.multiplier) & ")");
      Append(result, "(cas_cycles " & To_String(mem.cas_cycles) & ")");
      Append(result, "(rcd_cycles " & To_String(mem.rcd_cycles) & ")");
      Append(result, "(rp_cycles " & To_String(mem.rp_cycles) & ")");
      Append(result, "(wb_cycles " & To_String(mem.wb_cycles) & ")");
      Append(result, "(word_size " & To_String(mem.word_size) & ")");
      Append(result, "(page_size " & To_String(mem.page_size) & ")");
      Append(result, "(page_count " & To_String(mem.page_count) & ")");
      Append(result, "(width " & To_String(mem.width) & ")");
      Append(result, "(burst_size " & To_String(mem.burst_size) & ")");
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
