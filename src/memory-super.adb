
package body Memory.Super is

   function Create_Memory(size   : Natural;
                          next   : Memory_Pointer) return Memory_Pointer is
   begin
      return next;
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
         mem.dram_container := null;

      end if;

      -- Create the container for the DRAM.
      mem.dram_container := Create_Container(mem.dram);

      -- Create levels of SRAM until we hit the max size.
      while left > 0 loop
         size := Random.Random(mem.generator) mod left;
         temp := Create_Memory(size, temp);
         left := left - size;
      end loop;
      mem.sram := temp;

   end Randomize;

   function Create_Super(sram_size  : Natural;
                         dram       : access Memory_Type'Class)
                         return Super_Pointer is
      result : constant Super_Pointer := new Super_Type;
   begin
      result.sram_size := sram_size;
      result.dram := Memory_Pointer(dram);
      Randomize(Super_Type(result.all));
      return result;
   end Create_Super;

   procedure Reset(mem : in out Super_Type) is
   begin
      Randomize(mem);
   end Reset;

   procedure Finalize(mem : in out Super_Type) is
   begin
      if mem.dram /= null then
         Destroy(mem.dram);
      end if;
      if mem.sram /= null then
         Destroy(mem.sram);
      end if;
   end Finalize;

end Memory.Super;
