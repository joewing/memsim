
with Ada.Text_IO; use Ada.Text_IO;
with Memory.Cache;

package body Memory.Super is

   function Create_Cache(mem     : Super_Type;
                         size    : Natural;
                         next    : Memory_Pointer) return Memory_Pointer is
      line_count     : Positive;
      line_size      : Positive;
      latency        : constant Time_Type := 1;
      associativity  : Positive;
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
      associativity := 1 + (Random.Random(mem.generator) mod line_count);

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

   function Create_Memory(mem    : Super_Type;
                          size   : Natural;
                          next   : Memory_Pointer) return Memory_Pointer is
   begin
      return Create_Cache(mem, size, next);
   end Create_Memory;

   procedure Randomize(mem : in out Super_Type) is
      temp  : Memory_Pointer := null;
      left  : Natural := mem.sram_size;
      size  : Natural;
   begin

      -- Destroy the previous memory.
      if mem.sram /= null then

         -- Clear the DRAM container so that deleting the SRAM
         -- does not cause our DRAM to disappear.
         Set_Memory(mem.dram_container.all, null);

         -- Delete the SRAM.
         -- This will also delete the dram_container.
         Destroy(mem.sram);

      end if;

      -- Create the container for the DRAM.
      mem.dram_container := Create_Container(mem.dram);

      -- Create levels of SRAM until we hit the max size.
      temp := Memory_Pointer(mem.dram_container);
      while left > 0 loop
         size := 1 + (Random.Random(mem.generator) mod left);
         temp := Create_Memory(mem, size, temp);
         left := left - size;
      end loop;
      mem.sram := temp;

      Put_Line(To_String(To_String(mem.sram.all)));

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
      time : constant Time_Type := Get_Time(mem.sram.all);
   begin
      if time < mem.best_time then
         mem.best_time := time;
         mem.best_name := To_String(mem.sram.all);
      end if;
   end Finish_Run;

   procedure Reset(mem : in out Super_Type) is
   begin
      if mem.sram /= null then
         Finish_Run(mem);
      end if;
      Randomize(mem);
   end Reset;

   procedure Show_Access_Stats(mem : in Super_Type) is
   begin
      Put_Line("Best Memory: " & To_String(mem.best_name));
      Put_Line("Best Time:   " & Time_Type'Image(mem.best_time));
   end Show_Access_Stats;

   procedure Finalize(mem : in out Super_Type) is
   begin
      if mem.dram /= null then
         Destroy(mem.dram);
      end if;
      if mem.sram /= null then
         Finish_Run(mem);
         Set_Memory(mem.dram_container.all, null);
         Destroy(mem.sram);
      end if;
   end Finalize;

end Memory.Super;
