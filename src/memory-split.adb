
with Ada.Assertions; use Ada.Assertions;

with Random_Enum;

package body Memory.Split is

   function Random_Boolean is new Random_Enum(Boolean);

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
         Set_Split(banka.all, result);
      end if;
      if bankb /= null then
         Set_Split(bankb.all, result);
      end if;
      return result;
   end Create_Split;

   function Random_Split(generator  : RNG.Generator;
                         max_cost   : Cost_Type)
                         return Memory_Pointer is
      result : constant Split_Pointer := new Split_Type;
   begin
      result.offset := 8;
      return Memory_Pointer(result);
   end Random_Split;

   function Clone(mem : Split_Type) return Memory_Pointer is
      result : constant Split_Pointer := new Split_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Split_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      if mem.offset > 1 and then Random_Boolean(RNG.Random(generator)) then
         mem.offset := mem.offset / 2;
      else
         mem.offset := mem.offset * 2;
      end if;
   end Permute;

   function Get_Bank(mem   : Split_Type'Class;
                     index : Natural) return Memory_Pointer is
   begin
      Assert(index < 2);
      return Memory_Pointer(mem.banks(index).mem);
   end Get_Bank;

   procedure Set_Bank(mem     : in out Split_Type'Class;
                      index   : in Natural;
                      other   : access Memory_Type'Class) is
   begin
      Assert(index < 2);
      mem.banks(index).mem := other;
      Set_Split(other.all, mem'Access);
   end Set_Bank;

   procedure Reset(mem : in out Split_Type) is
   begin
      Reset(Memory_Type(mem));
      for i in mem.banks'Range loop
         Reset(mem.banks(i).mem.all);
         mem.banks(i).pending := 0;
      end loop;
   end Reset;

   procedure Process(mem      : in out Split_Type;
                     address  : in Address_Type;
                     size     : in Positive;
                     is_read  : in Boolean) is
      last        : constant Address_Type := address + Address_Type(size);
      start_time  : Time_Type;
      temp_addr   : Address_Type;
      temp_size   : Positive;
   begin
      if address < mem.offset then
         if last <= mem.offset then
            temp_size := size;
         else
            temp_size := Positive(mem.offset - address);
         end if;
         Advance(mem, mem.banks(0).pending);
         start_time := Get_Time(mem.banks(0).mem.all);
         if is_read then
            Read(mem.banks(0).mem.all, address, temp_size);
         else
            Write(mem.banks(0).mem.all, address, temp_size);
         end if;
         mem.banks(0).pending := Get_Time(mem.banks(0).mem.all) - start_time;
      end if;
      if last >= mem.offset then
         if address >= mem.offset then
            temp_addr := address;
            temp_size := size;
         else
            temp_addr := mem.offset;
            temp_size := Positive(last - mem.offset + 1);
         end if;
         Advance(mem, mem.banks(1).pending);
         start_time := Get_Time(mem.banks(1).mem.all);
         if is_read then
            Read(mem.banks(1).mem.all, temp_addr, temp_size);
         else
            Write(mem.banks(1).mem.all, temp_addr, temp_size);
         end if;
         mem.banks(1).pending := Get_Time(mem.banks(1).mem.all) - start_time;
      end if;
   end Process;

   procedure Read(mem      : in out Split_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Process(mem, address, size, True);
   end Read;

   procedure Write(mem     : in out Split_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Process(mem, address, size, False);
   end Write;

   procedure Idle(mem      : in out Split_Type;
                  cycles   : in Time_Type) is
   begin
      for i in mem.banks'Range loop
         if mem.banks(i).pending >= cycles then
            mem.banks(i).pending := mem.banks(i).pending - cycles;
         else
            Idle(mem.banks(i).mem.all, cycles - mem.banks(i).pending);
            mem.banks(i).pending := 0;
         end if;
      end loop;
      Advance(mem, cycles);
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
      Append(result, "(offset " & Address_Type'Image(mem.offset) & ")");
      Append(result, "(memory0 ");
      Append(result, To_String(mem.banks(0).mem.all));
      Append(result, ")");
      Append(result, "(memory1 ");
      Append(result, To_String(mem.banks(1).mem.all));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Split_Type) return Cost_Type is
      result   : Cost_Type := 0;
   begin
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
      for i in mem.banks'Range loop
         mem.banks(i).mem := Clone(mem.banks(i).mem.all);
      end loop;
   end Adjust;

   procedure Finalize(mem : in out Split_Type) is
   begin
      for i in mem.banks'Range loop
         Destroy(Memory_Pointer(mem.banks(i).mem));
      end loop;
   end Finalize;

end Memory.Split;

