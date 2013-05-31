
package Memory.Transform.Shift is

   type Shift_Type is new Transform_Type with private;

   type Shift_Pointer is access all Shift_Type'Class;

   function Create_Shift return Shift_Pointer;

   function Random_Shift(next       : access Memory_Type'Class;
                         generator  : RNG.Generator;
                         max_cost   : Cost_Type) return Memory_Pointer;

   function Get_Shift(mem : Shift_Type) return Integer;

   procedure Set_Shift(mem    : in out Shift_Type;
                       shift  : in Integer);

   overriding
   function Clone(mem : Shift_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Shift_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   overriding
   function To_String(mem : Shift_Type) return Unbounded_String;

   overriding
   procedure Generate(mem  : in Shift_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

private

   type Shift_Type is new Transform_Type with record
      shift : Natural;
   end record;

   overriding
   function Apply(mem      : Shift_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

end Memory.Transform.Shift;
