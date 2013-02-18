
with Ada.Numerics.Discrete_Random;
with Memory.Container; use Memory.Container;

package Memory.Super is

   type Super_Type is new Container_Type with private;

   type Super_Pointer is access all Super_Type'Class;

   function Create_Super(sram_size  : Natural;
                         dram       : access Memory_Type'Class)
                         return Super_Pointer;

   overriding
   procedure Reset(mem : in out Super_Type);

   overriding
   procedure Finalize(mem : in out Super_Type);

private

   package Random is new Ada.Numerics.Discrete_Random(Natural);

   type Super_Type is new Container_Type with record
      sram_size   : Natural;
      dram        : Memory_Pointer;
      generator   : Random.Generator;
   end record;

end Memory.Super;
