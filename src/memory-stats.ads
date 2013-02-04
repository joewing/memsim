
with Ada.Containers.Vectors;
use Ada.Containers;

package Memory.Stats is

   type Stats_Type is new Memory_Type with private;

   type Stats_Pointer is access all Stats_Type'class;

   function Create_Stats(mem : access Memory_Type'class) return Stats_Pointer;

   overriding
   procedure Read(mem      : in out Stats_Type;
                  address  : in Address_Type);

   overriding
   procedure Write(mem     : in out Stats_Type;
                   address : in Address_Type);

   overriding
   procedure Show_Stats(mem : in Stats_Type);

private

   package Natural_Vectors is new Vectors(Natural, Natural);

   type Stats_Type is new Memory_Type with record
      mem         : access Memory_Type'class := null;
      reads       : Natural := 0;
      writes      : Natural := 0;
      strides     : Natural_Vectors.Vector;
      multipliers : Natural_Vectors.Vector;
   end record;

end Memory.Stats;
