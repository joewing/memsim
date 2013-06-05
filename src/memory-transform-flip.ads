
package Memory.Transform.Flip is

   type Flip_Type is new Transform_Type with private;

   type Flip_Pointer is access all Flip_Type'Class;

   function Create_Flip return Flip_Pointer;

   function Random_Flip(next        : access Memory_Type'Class;
                        generator   : RNG.Generator;
                        max_cost    : Cost_Type) return Memory_Pointer;

   overriding
   function Clone(mem : Flip_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Flip_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   overriding
   function To_String(mem : Flip_Type) return Unbounded_String;

   overriding
   procedure Generate_Simple(mem    : in Flip_Type;
                             sigs   : in out Unbounded_String;
                             code   : in out Unbounded_String);

   overriding
   procedure Generate_Banked(mem    : in Flip_Type;
                             sigs   : in out Unbounded_String;
                             code   : in out Unbounded_String);

private

   type Flip_Type is new Transform_Type with null record;

   overriding
   function Apply(mem      : Flip_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

   overriding
   function Get_Alignment(mem : Flip_Type) return Positive;

end Memory.Transform.Flip;
