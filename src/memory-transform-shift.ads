
package Memory.Transform.Shift is

   type Shift_Type is new Transform_Type with private;

   type Shift_Pointer is access all Shift_Type'Class;

   function Create_Shift return Shift_Pointer;

   function Random_Shift(next       : access Memory_Type'Class;
                         generator  : Distribution_Type;
                         max_cost   : Cost_Type) return Memory_Pointer;

   overriding
   function Clone(mem : Shift_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Shift_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   function Is_Empty(mem : Shift_Type) return Boolean;

   overriding
   function Get_Name(mem : Shift_Type) return String;

private

   type Shift_Type is new Transform_Type with null record;

   overriding
   function Apply(mem      : Shift_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

   overriding
   function Get_Alignment(mem : Shift_Type) return Positive;

   overriding
   function Get_Transform_Length(mem : Shift_Type) return Natural;

end Memory.Transform.Shift;
