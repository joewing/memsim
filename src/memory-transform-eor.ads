
package Memory.Transform.EOR is

   type EOR_Type is new Transform_Type with private;

   type EOR_Pointer is access all EOR_Type'Class;

   function Create_EOR return EOR_Pointer;

   function Random_EOR(next      : access Memory_Type'Class;
                       generator : RNG.Generator;
                       max_cost  : Cost_Type) return Memory_Pointer;

   overriding
   function Clone(mem : EOR_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out EOR_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   overriding
   function To_String(mem : EOR_Type) return Unbounded_String;

   overriding
   procedure Generate_Simple(mem  : in EOR_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String);

   overriding
   procedure Generate_Banked(mem  : in EOR_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String);

   overriding
   function Is_Empty(mem : EOR_Type) return Boolean;

private

   type EOR_Type is new Transform_Type with null record;

   overriding
   function Apply(mem      : EOR_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type;

   overriding
   function Get_Alignment(mem : EOR_Type) return Positive;

end Memory.Transform.EOR;