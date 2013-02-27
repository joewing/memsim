
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Memory.Container; use Memory.Container;

generic
   type Value_Type is range <>;
   with function Get_Value(mem : access Memory_Type'Class) return Value_Type;
package Memory.Super is

   type Super_Type is new Container_Type with private;

   type Super_Pointer is access all Super_Type'Class;

   function Create_Super(mem        : not null access Memory_Type'Class;
                         max_cost   : Cost_Type;
                         seed       : Integer)
                         return Super_Pointer;

   overriding
   function Clone(mem : Super_Type) return Memory_Pointer;

   overriding
   procedure Show_Access_Stats(mem : in out Super_Type);

   overriding
   procedure Adjust(mem : in out Super_Type);

   overriding
   procedure Finalize(mem : in out Super_Type);

private

   package Memory_Vectors is new Ada.Containers.Vectors(Natural,
                                                        Container_Pointer);

   package Value_Maps is new Ada.Containers.Ordered_Maps(Unbounded_String,
                                                         Value_Type);

   type Super_Type is new Container_Type with record
      max_cost       : Cost_Type             := 1e6;
      dram           : Memory_Pointer        := null;
      generator      : Generator_Pointer     := new RNG.Generator;
      best_name      : Unbounded_String      := Null_Unbounded_String;
      best_cost      : Cost_Type             := Cost_Type'Last;
      best_value     : Value_Type            := Value_Type'Last;
      chain          : Memory_Vectors.Vector;
      last_chain     : Memory_Vectors.Vector;
      last_cost      : Cost_Type             := Cost_Type'Last;
      last_value     : Value_Type            := Value_Type'Last;
      table          : Value_Maps.Map;
   end record;

end Memory.Super;
