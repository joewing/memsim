
package Memory.Transform.Offset is

   type Offset_Type is new Transform_Type with private;

   type Offset_Pointer is access all Offset_Type'Class;

   function Create_Offset return Offset_Pointer;

   function Random_Offset(next      : access Memory_Type'Class;
                          generator : Distribution_Type;
                          max_cost  : Cost_Type) return Memory_Pointer;

   overriding
   function Clone(mem : Offset_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Offset_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   function Is_Empty(mem : Offset_Type) return Boolean;

   overriding
   function Get_Name(mem : Offset_Type) return String;

private

   type Offset_Type is new Transform_Type with null record;

   overriding
   function Apply(mem      : Offset_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

   overriding
   function Get_Alignment(mem : Offset_Type) return Positive;

   overriding
   function Get_Transform_Length(mem : Offset_Type) return Natural;

end Memory.Transform.Offset;
