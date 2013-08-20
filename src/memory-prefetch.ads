
with Memory.Container; use Memory.Container;

package Memory.Prefetch is

   type Prefetch_Type is new Container_Type with private;

   type Prefetch_Pointer is access all Prefetch_Type'Class;

   function Create_Prefetch(mem        : access Memory_Type'Class;
                            stride     : Address_Type := 1)
                            return Prefetch_Pointer;

   function Random_Prefetch(next       : access Memory_Type'Class;
                            generator  : Distribution_Type;
                            max_cost   : Cost_Type)
                            return Memory_Pointer;

   overriding
   function Clone(mem : Prefetch_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Prefetch_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   procedure Reset(mem     : in out Prefetch_Type;
                   context : in Natural);

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
   function Get_Time(mem : Prefetch_Type) return Time_Type;

   overriding
   function To_String(mem : Prefetch_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Prefetch_Type) return Cost_Type;

   overriding
   procedure Generate(mem  : in Prefetch_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

private

   type Prefetch_Type is new Container_Type with record
      pending     : Time_Type := 0;
      stride      : Address_Type := 1;
   end record;

end Memory.Prefetch;
