
package body Memory.Super is

   procedure Randomize(mem : in out Super_Type) is
      sram  : Memory_Pointer := null;
      left  : Natural := mem.sram_size;
      size  : Natural;
   begin
      while left > 0 loop
         size := Random.Random(mem.generator) mod left;
         left := left - size;
      end loop;
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
      Destroy(mem.dram);
   end Finalize;

end Memory.Super;
