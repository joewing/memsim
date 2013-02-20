
with Ada.Text_IO;             use Ada.Text_IO;
with Memory.Cache;            use Memory.Cache;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Transform.Shift;  use Memory.Transform.Shift;
with Benchmark;

package body Memory.Super is

   function Create_Cache(mem  : Super_Type;
                         cost : Cost_Type;
                         next : Memory_Pointer) return Memory_Pointer is
   begin
      return Memory_Pointer(Random_Cache(next, mem.generator, cost));
   end Create_Cache;

   function Create_Transform(mem    : Super_Type;
                             next   : Memory_Pointer) return Memory_Pointer is
      offset   : Integer;
      shift    : Natural;
      result   : Memory_Pointer;
   begin
      case RNG.Random(mem.generator) mod 4 is
         when 0 =>      -- None
            result := next;
         when 1 =>      -- Offset
            offset := Integer(RNG.Random(mem.generator)) - Integer'Last / 2;
            result := Memory_Pointer(Create_Offset(next, offset));
         when 2 =>      -- Shift
            shift := RNG.Random(mem.generator) mod Address_Type'Size;
            result := Memory_Pointer(Create_Shift(next, shift));
         when others => -- Offset + Shift
            offset := Integer(RNG.Random(mem.generator)) - Integer'Last / 2;
            shift := RNG.Random(mem.generator) mod Address_Type'Size;
            result := Memory_Pointer(Create_Offset(next, offset));
            result := Memory_Pointer(Create_Shift(result, shift));
      end case;
      return result;
   end Create_Transform;

   function Create_Memory(mem    : Super_Type;
                          cost   : Cost_Type;
                          next   : Memory_Pointer) return Memory_Pointer is
   begin
      return Create_Cache(mem, cost, Create_Transform(mem, next));
   end Create_Memory;

   procedure Randomize(mem : in out Super_Type) is
      temp     : Memory_Pointer := null;
   begin

      -- Clear the DRAM container so that deleting the SRAM
      -- does not cause our DRAM to disappear.  The SRAM
      -- will be deleted when a new SRAM is set.
      if mem.dram_container /= null then
         Set_Memory(mem.dram_container.all, null);
      end if;

      -- Create the container for the DRAM.
      mem.dram_container := Create_Container(mem.dram);

      -- Randomly add more levels.
      temp := Memory_Pointer(mem.dram_container);
      loop
         declare
            left : constant Cost_Type := mem.max_cost - Get_Cost(temp.all);
         begin
            temp := Create_Memory(mem, left, temp);
         end;
         exit when (RNG.Random(mem.generator) mod 2) = 0;
      end loop;
      Set_Memory(mem, temp);

      Put_Line(To_String(To_String(mem)));

   end Randomize;

   function Create_Super(max_cost   : Cost_Type;
                         dram       : access Memory_Type'Class)
                         return Super_Pointer is
      result : constant Super_Pointer := new Super_Type;
   begin
      result.max_cost := max_cost;
      result.dram := Memory_Pointer(dram);
      return result;
   end Create_Super;

   procedure Finish_Run(mem : in out Super_Type) is
      time : constant Time_Type := Get_Time(mem);
      cost : constant Cost_Type := Get_Cost(mem);
   begin
      if time > 0 then
         if time < mem.best_time or else
            (time = mem.best_time and cost < mem.best_cost) then
            mem.best_time := time;
            mem.best_cost := cost;
            mem.best_name := To_String(mem);
         end if;
      end if;
   end Finish_Run;

   procedure Reset(mem : in out Super_Type) is
   begin
      Reset(Container_Type(mem));
      if mem.dram /= null then
         Reset(mem.dram.all);
      end if;
      Randomize(mem);
   end Reset;

   procedure Read(mem      : in out Super_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Read(Container_Type(mem), address, size);
      if Get_Time(mem) >= mem.best_time then
         raise Benchmark.Timeout;
      end if;
   end Read;

   procedure Write(mem     : in out Super_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Write(Container_Type(mem), address, size);
      if Get_Time(mem) >= mem.best_time then
         raise Benchmark.Timeout;
      end if;
   end Write;

   procedure Idle(mem      : in out Super_Type;
                  cycles   : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
      if Get_Time(mem) >= mem.best_time then
         raise Benchmark.Timeout;
      end if;
   end Idle;

   procedure Show_Access_Stats(mem : in out Super_Type) is
   begin
      Finish_Run(mem);
      Put_Line("Best Memory: " & To_String(mem.best_name));
      Put_Line("Best Time:   " & Time_Type'Image(mem.best_time));
   end Show_Access_Stats;

   procedure Finalize(mem : in out Super_Type) is
   begin
      if mem.dram /= null then
         Destroy(mem.dram);
         if mem.dram_container /= null then
            Set_Memory(mem.dram_container.all, null);
         end if;
      end if;
   end Finalize;

end Memory.Super;
