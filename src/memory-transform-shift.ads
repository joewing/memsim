
package Memory.Transform.Shift is

   type Shift_Type is new Transform_Type with private;

   type Shift_Pointer is access all Shift_Type'Class;

   function Create_Shift return Shift_Pointer;

   function Random_Shift(next       : access Memory_Type'Class;
                         generator  : RNG.Generator;
                         max_cost   : Cost_Type) return Memory_Pointer;

   overriding
   function Clone(mem : Shift_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Shift_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   overriding
   function To_String(mem : Shift_Type) return Unbounded_String;

   overriding
   procedure Generate_Simple(mem  : in Shift_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String);

   overriding
   procedure Generate_Banked(mem  : in Shift_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String);

   overriding
   function Is_Empty(mem : Shift_Type) return Boolean;

private

   type Shift_Type is new Transform_Type with null record;

   overriding
   function Apply(mem      : Shift_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

   overriding
   function Get_Alignment(mem : Shift_Type) return Positive;

end Memory.Transform.Shift;
