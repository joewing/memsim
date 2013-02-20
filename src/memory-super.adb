
with Ada.Text_IO; use Ada.Text_IO;
with Memory.Cache;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Transform.Shift; use Memory.Transform.Shift;
with Benchmark;

package body Memory.Super is

   function Create_Cache(mem     : Super_Type;
                         size    : Natural;
                         next    : Memory_Pointer) return Memory_Pointer is
      line_count     : Positive;
      line_size      : Positive;
      latency        : constant Time_Type := 1;
      associativity  : Natural;
      policy         : Cache.Policy_Type;
      result         : Cache.Cache_Pointer;
   begin

      -- Select a random line size and count.
      loop
         line_count  := 1 + (Random.Random(mem.generator) mod size);
         line_size   := size / line_count;
         exit when line_size * line_count = size;
      end loop;

      -- Select a cache associativity.
      associativity := Random.Random(mem.generator) mod Log2(line_count);
      associativity := 2 ** associativity;

      -- Select a policy.
      case Random.Random(mem.generator) mod 4 is
         when 0      => policy := Cache.LRU;
         when 1      => policy := Cache.MRU;
         when 2      => policy := Cache.FIFO;
         when others => policy := Cache.Random;
      end case;

      result := Cache.Create_Cache(next, line_count, line_size,
                                   associativity, latency, policy);

      return Memory_Pointer(result);
   end Create_Cache;

   function Create_Transform(mem    : Super_Type;
                             next   : Memory_Pointer) return Memory_Pointer is
      offset   : Integer;
      shift    : Natural;
      result   : Memory_Pointer;
   begin
      case Random.Random(mem.generator) mod 4 is
         when 0 =>      -- None
            result := next;
         when 1 =>      -- Offset
            offset := Integer(Random.Random(mem.generator)) - Integer'Last / 2;
            result := Memory_Pointer(Create_Offset(next, offset));
         when 2 =>      -- Shift
            shift := Random.Random(mem.generator) mod Address_Type'Size;
            result := Memory_Pointer(Create_Shift(next, shift));
         when others => -- Offset + Shift
            offset := Integer(Random.Random(mem.generator)) - Integer'Last / 2;
            shift := Random.Random(mem.generator) mod Address_Type'Size;
            result := Memory_Pointer(Create_Offset(next, offset));
            result := Memory_Pointer(Create_Shift(result, shift));
      end case;
      return result;
   end Create_Transform;

   function Create_Memory(mem    : Super_Type;
                          size   : Natural;
                          next   : Memory_Pointer) return Memory_Pointer is
   begin
      return Create_Cache(mem, size, Create_Transform(mem, next));
   end Create_Memory;

   procedure Randomize(mem : in out Super_Type) is
      temp  : Memory_Pointer := null;
      left  : Natural := mem.sram_size;
      size  : Natural;
   begin

      -- Clear the DRAM container so that deleting the SRAM
      -- does not cause our DRAM to disappear.  The SRAM
      -- will be deleted when a new SRAM is set.
      if mem.dram_container /= null then
         Set_Memory(mem.dram_container.all, null);
      end if;

      -- Create the container for the DRAM.
      mem.dram_container := Create_Container(mem.dram);

      -- Create levels of SRAM until we hit the max size.
      temp := Memory_Pointer(mem.dram_container);
      while left > 0 loop
         declare
            bits_left   : constant Natural := Log2(left);
            bits        : constant Natural
               := Random.Random(mem.generator) mod bits_left;
         begin
            size := 2 ** bits;
            temp := Create_Memory(mem, size, temp);
            left := left - size;
         end;
      end loop;
      Set_Memory(mem, temp);

      Put_Line(To_String(To_String(mem)));

   end Randomize;

   function Create_Super(sram_size  : Natural;
                         dram       : access Memory_Type'Class)
                         return Super_Pointer is
      result : constant Super_Pointer := new Super_Type;
   begin
      result.sram_size := sram_size;
      result.dram := Memory_Pointer(dram);
      return result;
   end Create_Super;

   procedure Finish_Run(mem : in out Super_Type) is
      time : constant Time_Type := Get_Time(mem);
   begin
      if time > 0 and time < mem.best_time then
         mem.best_time := time;
         mem.best_name := To_String(mem);
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
