
with Ada.Containers.Ordered_Maps;
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
                         max_iterations   : Long_Integer)
                         return Super_Pointer;
   overriding
   function Done(mem : Super_Type) return Boolean;

   overriding
   function Clone(mem : Super_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem : in out Super_Type);

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
   procedure Show_Access_Stats(mem : in out Super_Type);

   overriding
   procedure Adjust(mem : in out Super_Type);

   overriding
   procedure Finalize(mem : in out Super_Type);

private

   package Value_Maps is new Ada.Containers.Ordered_Maps(Unbounded_String,
                                                         Value_Type);

   type Super_Type is new Container_Type with record
      max_cost       : Cost_Type             := 1e6;
      generator      : Distribution_Pointer;

      total_length   : Natural               := 0;
      current_length : Natural               := 0;

      best_name      : Unbounded_String      := Null_Unbounded_String;
      best_cost      : Cost_Type             := Cost_Type'Last;
      best_value     : Value_Type            := Value_Type'Last;

      last           : Memory_Pointer        := null;
      current        : Memory_Pointer        := null;

      last_value     : Value_Type            := Value_Type'Last;
      table          : Value_Maps.Map;
      iteration      : Long_Integer          := 0;
      total          : Long_Integer          := 0;
      max_iterations : Long_Integer          := 1000;
      steps          : Long_Integer          := 0;

      threshold      : Long_Integer          := 0;

      has_idle       : Boolean               := False;

   end record;

end Memory.Super;
