
with Ada.Containers.Ordered_Maps;
use Ada.Containers;
with Memory.Container; use Memory.Container;

generic
   type Value_Type is range <>;
   with function Get_Value(mem : access Memory_Type'Class) return Value_Type;
package Memory.Super is

   type Super_Type is new Container_Type with private;

   type Super_Pointer is access all Super_Type'Class;

   function Create_Super(mem              : not null access Memory_Type'Class;
                         max_cost         : Cost_Type;
                         seed             : Integer;
                         max_iterations   : Long_Integer;
                         permute_only     : Boolean)
                         return Super_Pointer;
   overriding
   function Done(mem : Super_Type) return Boolean;

   overriding
   function Clone(mem : Super_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem     : in out Super_Type;
                   context : in Natural);

   overriding
   procedure Read(mem      : in out Super_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Super_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Super_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Stats(mem : in out Super_Type);

   overriding
   procedure Show_Access_Stats(mem : in out Super_Type);

   overriding
   procedure Adjust(mem : in out Super_Type);

   overriding
   procedure Finalize(mem : in out Super_Type);

private

   package Value_Vectors is new Vectors(Natural, Value_Type);

   type Result_Type is record
      value          : Value_Type;
      context_values : Value_Vectors.Vector;
   end record;

   package Value_Maps is new Ordered_Maps(Unbounded_String, Result_Type);

   type Context_Type is record
      index          : Natural      := 0;
      value          : Value_Type   := Value_Type'Last;
      last_value     : Value_Type   := Value_Type'Last;
      total_length   : Long_Integer := 0;
   end record;

   type Context_Pointer is access all Context_Type;

   package Context_Vectors is new Vectors(Natural, Context_Pointer);

   type Super_Type is new Container_Type with record
      max_cost       : Cost_Type                := 1e6;
      permute_only   : Boolean                  := False;
      generator      : Distribution_Pointer     := null;
      current_length : Long_Integer             := 0;
      best_name      : Unbounded_String         := Null_Unbounded_String;
      best_cost      : Cost_Type                := Cost_Type'Last;
      best_value     : Value_Type               := Value_Type'Last;
      last           : Memory_Pointer           := null;
      current        : Memory_Pointer           := null;
      contexts       : Context_Vectors.Vector;
      context        : Context_Pointer          := null;
      last_value     : Value_Type               := Value_Type'Last;
      table          : Value_Maps.Map;
      total          : Long_Integer             := 0;
      max_iterations : Long_Integer             := 1000;
      steps          : Long_Integer             := 0;
      iteration      : Long_Integer             := 0;
      improvement    : Value_Type               := 0;
      threshold      : Long_Integer             := 0;
      has_idle       : Boolean                  := False;
      age            : Long_Integer             := 0;
   end record;

end Memory.Super;
