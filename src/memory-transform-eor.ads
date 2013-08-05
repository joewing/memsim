
package Memory.Transform.EOR is

   type EOR_Type is new Transform_Type with private;

   type EOR_Pointer is access all EOR_Type'Class;

   function Create_EOR return EOR_Pointer;

   function Random_EOR(next      : access Memory_Type'Class;
                       generator : Distribution_Type;
                       max_cost  : Cost_Type) return Memory_Pointer;

   overriding
   function Clone(mem : EOR_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out EOR_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   function Is_Empty(mem : EOR_Type) return Boolean;

   overriding
   function Get_Name(mem : EOR_Type) return String;

private

   type EOR_Type is new Transform_Type with null record;

   overriding
   function Apply(mem      : EOR_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

   overriding
   function Get_Alignment(mem : EOR_Type) return Positive;

   overriding
   function Get_Transform_Length(mem : EOR_Type) return Natural;

end Memory.Transform.EOR;
