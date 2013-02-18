
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
   procedure Show_Access_Stats(mem : in Super_Type);

   overriding
   procedure Finalize(mem : in out Super_Type);

private

   package Random is new Ada.Numerics.Discrete_Random(Natural);

   type Super_Type is new Container_Type with record
      sram_size      : Natural            := 0;
      dram           : Memory_Pointer     := null;
      dram_container : Container_Pointer  := null;
      generator      : Random.Generator;
      best_name      : Unbounded_String;
      best_time      : Time_Type          := Time_Type'Last;
   end record;

end Memory.Super;
