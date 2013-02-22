
package Memory.Prefetch is

   type Prefetch_Type is new Memory_Type with private;

   type Prefetch_Pointer is access all Prefetch_Type'Class;

   function Create_Prefetch(mem        : access Memory_Type'Class;
                            stride     : Address_Type := 1;
                            multiplier : Address_Type := 1)
                            return Prefetch_Pointer;

   function Clone(mem : Prefetch_Type) return Memory_Pointer;

   overriding
   procedure Read(mem      : in out Prefetch_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Prefetch_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Prefetch_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Access_Stats(mem : in out Prefetch_Type);

   overriding
   function To_String(mem : Prefetch_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Prefetch_Type) return Cost_Type;

   overriding
   procedure Adjust(mem : in out Prefetch_Type);

   overriding
   procedure Finalize(mem : in out Prefetch_Type);

private

   type Prefetch_Type is new Memory_Type with record
      mem         : access Memory_Type'Class;
      pending     : Time_Type := 0;
      stride      : Address_Type := 1;
      multiplier  : Address_Type := 1;
   end record;

end Memory.Prefetch;
