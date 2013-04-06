
with Ada.Assertions; use Ada.Assertions;

package body Memory.Split is

   function Create_Split(mem     : access Memory_Type'Class;
                         banka   : access Memory_Type'Class;
                         bankb   : access Memory_Type'Class;
                         offset  : Address_Type) return Split_Pointer is
      result : constant Split_Pointer := new Split_Type;
   begin
      Set_Memory(result.all, mem);
      result.banks(0).mem := banka;
      result.banks(1).mem := bankb;
      result.offset := offset;
      if banka /= null then
         Set_Split(banka.all, 0, Memory_Pointer(result));
      end if;
      if bankb /= null then
         Set_Split(bankb.all, 1, Memory_Pointer(result));
      end if;
      return result;
   end Create_Split;

   function Random_Split(generator  : RNG.Generator;
                         max_cost   : Cost_Type)
                         return Memory_Pointer is
      result : constant Split_Pointer := new Split_Type;
   begin
      result.offset := 2 ** ((RNG.Random(generator) mod 16) + 3);
      return Memory_Pointer(result);
   end Random_Split;

   function Clone(mem : Split_Type) return Memory_Pointer is
      result : constant Split_Pointer := new Split_Type'(mem);
   begin
      for i in result.banks'Range loop
         Set_Split(result.banks(i).mem.all, i, Memory_Pointer(result));
      end loop;
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Split_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      if mem.offset > 1 and then (RNG.Random(generator) mod 2) = 0 then
         mem.offset := mem.offset / 2;
      else
         mem.offset := mem.offset * 2;
      end if;
      Assert(Get_Cost(mem) <= max_cost, "Invalid Permute in Memory.Split");
   end Permute;

   function Get_Bank(mem   : Split_Pointer;
                     index : Natural) return Memory_Pointer is
   begin
      Assert(index < 2);
      return Memory_Pointer(mem.banks(index).mem);
   end Get_Bank;

   procedure Set_Bank(mem     : in Split_Pointer;
                      index   : in Natural;
                      other   : access Memory_Type'Class) is
   begin
      Assert(index < 2);
      mem.banks(index).mem := other;
      Set_Split(other.all, index, Memory_Pointer(mem));
   end Set_Bank;

   function Get_Offset(mem : Split_Type'Class) return Address_Type is
   begin
      return mem.offset;
   end Get_Offset;

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

   procedure Adjust(mem : in out Split_Type) is
   begin
      Adjust(Container_Type(mem));
      for i in mem.banks'Range loop
         mem.banks(i).mem := Clone(mem.banks(i).mem.all);
      end loop;
   end Adjust;

   procedure Finalize(mem : in out Split_Type) is
   begin
      Finalize(Container_Type(mem));
      for i in mem.banks'Range loop
         Destroy(Memory_Pointer(mem.banks(i).mem));
      end loop;
   end Finalize;

end Memory.Split;

