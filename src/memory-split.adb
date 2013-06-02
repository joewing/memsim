
with Ada.Assertions; use Ada.Assertions;
with Memory.Join;    use Memory.Join;

package body Memory.Split is

   function Create_Split return Split_Pointer is
      result : constant Split_Pointer := new Split_Type;
   begin
      return result;
   end Create_Split;

   function Random_Split(next       : access Memory_Type'Class;
                         generator  : RNG.Generator;
                         max_cost   : Cost_Type)
                         return Memory_Pointer is
      result   : constant Split_Pointer := Create_Split;
      wsize    : constant Natural := Get_Word_Size(next.all);
   begin
      Set_Memory(result.all, next);
      result.offset := Address_Type(2 ** (RNG.Random(generator) mod 16));
      result.offset := result.offset * Address_Type(wsize);
      return Memory_Pointer(result);
   end Random_Split;

   function Clone(mem : Split_Type) return Memory_Pointer is
      result   : constant Split_Pointer := new Split_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Split_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
      wsize : constant Address_Type := Address_Type(Get_Word_Size(mem));
   begin
      if mem.offset > wsize and then (RNG.Random(generator) mod 2) = 0 then
         mem.offset := mem.offset - wsize;
      else
         mem.offset := mem.offset + wsize;
      end if;
      Assert(Get_Cost(mem) <= max_cost, "Invalid Permute in Memory.Split");
   end Permute;

   function Get_Bank(mem   : Split_Type;
                     index : Natural) return Memory_Pointer is
   begin
      Assert(index < 2);
      return Memory_Pointer(mem.banks(index).mem);
   end Get_Bank;

   procedure Set_Bank(mem     : in out Split_Type;
                      index   : in Natural;
                      other   : access Memory_Type'Class) is
   begin
      Assert(index < 2);
      mem.banks(index).mem := other;
   end Set_Bank;

   function Get_Offset(mem : Split_Type'Class) return Address_Type is
   begin
      return mem.offset;
   end Get_Offset;

   procedure Set_Offset(mem      : in out Split_Type'Class;
                        offset   : in Address_Type) is
   begin
      mem.offset := offset;
   end Set_Offset;

   procedure Reset(mem : in out Split_Type) is
   begin
      Reset(Container_Type(mem));
      for i in mem.banks'Range loop
         Reset(mem.banks(i).mem.all);
      end loop;
   end Reset;

   procedure Do_Process(mem      : in out Split_Type;
                        address  : in Address_Type;
                        size     : in Positive;
                        is_read  : in Boolean) is
      last        : constant Address_Type := address + Address_Type(size) - 1;
      start_time  : Time_Type;
      temp_addr   : Address_Type;
      temp_size   : Positive;
   begin
      Assert(address <= last, "invalid address in Do_Process");
      if address < mem.offset then
         if last <= mem.offset then
            temp_size := size;
         else
            temp_size := Positive(mem.offset - address);
         end if;
         start_time := Get_Time(mem.banks(0).mem.all);
         if is_read then
            Read(mem.banks(0).mem.all, address, temp_size);
         else
            Write(mem.banks(0).mem.all, address, temp_size);
         end if;
         Advance(mem, Get_Time(mem.banks(0).mem.all) - start_time);
      end if;
      if last >= mem.offset then
         if address >= mem.offset then
            temp_addr := address - mem.offset;
            temp_size := size;
         else
            temp_addr := 0;
            temp_size := Positive(last - mem.offset + 1);
         end if;
         start_time := Get_Time(mem.banks(1).mem.all);
         if is_read then
            Read(mem.banks(1).mem.all, temp_addr, temp_size);
         else
            Write(mem.banks(1).mem.all, temp_addr, temp_size);
         end if;
         Advance(mem, Get_Time(mem.banks(1).mem.all) - start_time);
      end if;
   end Do_Process;

   procedure Process(mem      : in out Split_Type;
                     address  : in Address_Type;
                     size     : in Positive;
                     is_read  : in Boolean) is
      last : constant Address_Type := address + Address_Type(size - 1);
      temp : Positive;
   begin
      if address > last then

         temp := Positive(Address_Type'Last - address + 1);
         Do_Process(mem, address, temp, is_read);

         Do_Process(mem, 0, Positive(last + 1), is_read);

      else
         Do_Process(mem, address, size, is_read);
      end if;
   end Process;

   procedure Read(mem      : in out Split_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Assert(Get_Memory(mem) /= null, "Read");
      Process(mem, address, size, True);
   end Read;

   procedure Write(mem     : in out Split_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Assert(Get_Memory(mem) /= null, "Write");
      Process(mem, address, size, False);
   end Write;

   procedure Idle(mem      : in out Split_Type;
                  cycles   : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
      for i in mem.banks'Range loop
         Idle(mem.banks(i).mem.all, cycles);
      end loop;
   end Idle;

   procedure Show_Access_Stats(mem : in out Split_Type) is
   begin
      for i in mem.banks'Range loop
         Show_Access_Stats(mem.banks(i).mem.all);
      end loop;
   end Show_Access_Stats;

   function To_String(mem : Split_Type) return Unbounded_String is
      result   : Unbounded_String;
   begin
      Append(result, "(split ");
      Append(result, "(offset" & Address_Type'Image(mem.offset) & ")");
      if mem.banks(0).mem /= null then
         Append(result, "(bank0 ");
         Append(result, To_String(mem.banks(0).mem.all));
         Append(result, ")");
      end if;
      if mem.banks(1).mem /= null then
         Append(result, "(bank1 ");
         Append(result, To_String(mem.banks(1).mem.all));
         Append(result, ")");
      end if;
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Split_Type) return Cost_Type is
      result : Cost_Type;
   begin
      result := Get_Cost(Container_Type(mem));
      for i in mem.banks'Range loop
         result := result + Get_Cost(mem.banks(i).mem.all);
      end loop;
      return result;
   end Get_Cost;

   function Get_Writes(mem : Split_Type) return Long_Integer is
      result : Long_Integer := 0;
   begin
      for i in mem.banks'Range loop
         result := result + Get_Writes(mem.banks(i).mem.all);
      end loop;
      return result;
   end Get_Writes;

   function Find_Join(mem : Memory_Pointer) return Join_Pointer is
   begin
      if mem.all in Join_Type'Class then
         return Join_Pointer(mem);
      else
         declare
            cp : constant Container_Pointer := Container_Pointer(mem);
         begin
            return Find_Join(Get_Memory(cp.all));
         end;
      end if;
   end Find_Join;

   procedure Generate(mem  : in Split_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      other       : constant Memory_Pointer  := Get_Memory(mem);
      bank0       : constant Memory_Pointer  :=
                    Memory_Pointer(mem.banks(0).mem);
      bank1       : constant Memory_Pointer  :=
                    Memory_Pointer(mem.banks(1).mem);
      join0       : constant Join_Pointer    := Find_Join(bank0);
      join1       : constant Join_Pointer    := Find_Join(bank1);
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
      b0name      : constant String := "m" & To_String(Get_ID(bank0.all));
      b1name      : constant String := "m" & To_String(Get_ID(bank1.all));
      out0name    : constant String := "m" & To_String(Get_ID(join0.all));
      out1name    : constant String := "m" & To_String(Get_ID(join1.all));
      wsize       : constant Address_Type := Address_Type(Get_Word_Size(mem));
      offset      : constant Address_Type := mem.offset / wsize;
   begin

      Generate(other.all, sigs, code);

      -- Port into bank0 is b0name.
      -- Port out of bank0 is out0name.
      Generate(bank0.all, sigs, code);

      -- Port into bank1 is b1name.
      -- Port out of bank1 is out1name.
      Generate(bank1.all, sigs, code);

      Line(code, name & "_combine : entity work.combine");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH      => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      Line(code, "      OFFSET          => " & To_String(offset));
      Line(code, "   )");
      Line(code, "   port map (");
      Line(code, "      clk      => clk,");
      Line(code, "      rst      => rst,");
      Line(code, "      addr0    => " & out0name & "_addr,");
      Line(code, "      din0     => " & out0name & "_din,");
      Line(code, "      dout0    => " & out0name & "_dout,");
      Line(code, "      re0      => " & out0name & "_re,");
      Line(code, "      we0      => " & out0name & "_we,");
      Line(code, "      ready0   => " & out0name & "_ready,");
      Line(code, "      addr1    => " & out1name & "_addr,");
      Line(code, "      din1     => " & out1name & "_din,");
      Line(code, "      dout1    => " & out1name & "_dout,");
      Line(code, "      re1      => " & out1name & "_re,");
      Line(code, "      we1      => " & out1name & "_we,");
      Line(code, "      ready1   => " & out1name & "_ready,");
      Line(code, "      maddr    => " & oname & "_addr,");
      Line(code, "      mout     => " & oname & "_din,");
      Line(code, "      min      => " & oname & "_dout,");
      Line(code, "      mre      => " & oname & "_re,");
      Line(code, "      mwe      => " & oname & "_we,");
      Line(code, "      mready   => " & oname & "_ready");
      Line(code, "   );");

      -- Port into the split.
      Declare_Signals(sigs, name, word_bits);

      Line(code, name & "_sp : entity work.split");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH      => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      Line(code, "      OFFSET          => " & To_String(offset));
      Line(code, "   )");
      Line(code, "   port map (");
      Line(code, "      clk      => clk,");
      Line(code, "      rst      => rst,");
      Line(code, "      addr     => " & name & "_addr,");
      Line(code, "      din      => " & name & "_din,");
      Line(code, "      dout     => " & name & "_dout,");
      Line(code, "      re       => " & name & "_re,");
      Line(code, "      we       => " & name & "_we,");
      Line(code, "      ready    => " & name & "_ready,");
      Line(code, "      maddr0   => " & b0name & "_addr,");
      Line(code, "      mout0    => " & b0name & "_din,");
      Line(code, "      min0     => " & b0name & "_dout,");
      Line(code, "      mre0     => " & b0name & "_re,");
      Line(code, "      mwe0     => " & b0name & "_we,");
      Line(code, "      mready0  => " & b0name & "_ready,");
      Line(code, "      maddr1   => " & b1name & "_addr,");
      Line(code, "      mout1    => " & b1name & "_din,");
      Line(code, "      min1     => " & b1name & "_dout,");
      Line(code, "      mre1     => " & b1name & "_re,");
      Line(code, "      mwe1     => " & b1name & "_we,");
      Line(code, "      mready1  => " & b1name & "_ready");
      Line(code, "   );");

   end Generate;

   procedure Adjust(mem : in out Split_Type) is
      ptr   : Memory_Pointer;
      cp    : Container_Pointer;
      jp    : Join_Pointer;
   begin
      Adjust(Container_Type(mem));
      for i in mem.banks'Range loop
         mem.banks(i).mem := Clone(mem.banks(i).mem.all);
      end loop;
      for i in mem.banks'Range loop
         ptr := Memory_Pointer(mem.banks(i).mem);
         loop
            if ptr.all in Join_Type'Class then
               jp := Join_Pointer(ptr);
               Set_Parent(jp.all, mem'Unrestricted_Access);
               exit;
            else
               cp := Container_Pointer(ptr);
               ptr := Get_Memory(cp.all);
            end if;
         end loop;
      end loop;
   end Adjust;

   procedure Finalize(mem : in out Split_Type) is
   begin
      Finalize(Container_Type(mem));
      for i in mem.banks'Range loop
         Destroy(Memory_Pointer(mem.banks(i).mem));
      end loop;
   end Finalize;

   procedure Forward_Read(mem       : in out Split_Type;
                          source    : in Natural;
                          address   : in Address_Type;
                          size      : in Positive) is
      out_addr : Address_Type := address;
   begin
      if source = 1 then
         out_addr := address + mem.offset;
      end if;
      Read(Container_Type(mem), out_addr, size);
   end Forward_Read;

   procedure Forward_Write(mem      : in out Split_Type;
                           source   : in Natural;
                           address  : in Address_Type;
                           size     : in Positive) is
      out_addr : Address_Type := address;
   begin
      if source = 1 then
         out_addr := address + mem.offset;
      end if;
      Write(Container_Type(mem), out_addr, size);
   end Forward_Write;

   procedure Forward_Idle(mem       : in out Split_Type;
                          source    : in Natural;
                          cycles    : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
   end Forward_Idle;

   function Forward_Get_Time(mem : Split_Type) return Time_Type is
   begin
      return Get_Time(Container_Type(mem));
   end Forward_Get_Time;

end Memory.Split;
