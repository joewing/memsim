
package Memory.Transform.Flip is

   type Flip_Type is new Transform_Type with private;

   type Flip_Pointer is access all Flip_Type'Class;

   function Create_Flip return Flip_Pointer;

   function Random_Flip(next        : access Memory_Type'Class;
                        generator   : Distribution_Type;
                        max_cost    : Cost_Type) return Memory_Pointer;

   overriding
   function Clone(mem : Flip_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Flip_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   function Get_Name(mem : Flip_Type) return String;

private

   type Flip_Type is new Transform_Type with null record;

   overriding
   function Apply(mem      : Flip_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

   overriding
   function Get_Alignment(mem : Flip_Type) return Positive;

   overriding
   function Get_Transform_Length(mem : Flip_Type) return Natural;

end Memory.Transform.Flip;
