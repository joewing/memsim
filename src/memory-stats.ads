
with Ada.Containers.Ordered_Maps;

package Memory.Stats is

   type Stats_Type is new Memory_Type with private;

   type Stats_Pointer is access all Stats_Type'Class;

   function Create_Stats(mem : access Memory_Type'Class) return Stats_Pointer;

   overriding
   procedure Read(mem      : in out Stats_Type;
                  address  : in Address_Type);

   overriding
   procedure Write(mem     : in out Stats_Type;
                   address : in Address_Type);

private

   package Stride_Maps is new Ordered_Maps(Integer, Natural);

   type Stats_Type is new Memory_Type with record
      mem            : access Memory_Type'Class := null;
      last_address   : Integer := 0;
      last_stride    : Integer := 0;
      reads          : Natural := 0;
      writes         : Natural := 0;
      addresses      : Stride_Maps.Map;
      strides        : Stride_Maps.Map;
      multipliers    : Stride_Maps.Map;
   end record;

   overriding
   procedure Show_Access_Stats(mem : in Stats_Type);

   overriding
   procedure Finalize(mem : in out Stats_Type);

end Memory.Stats;
