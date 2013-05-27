
package Memory.Transform.Offset is

   type Offset_Type is new Transform_Type with private;

   type Offset_Pointer is access all Offset_Type'Class;

   function Create_Offset(mem    : access Memory_Type'Class;
                          offset : Integer) return Offset_Pointer;

   function Random_Offset(next      : access Memory_Type'Class;
                          generator : RNG.Generator;
                          max_cost  : Cost_Type) return Memory_Pointer;

   function Get_Offset(mem : Offset_Type) return Address_Type;

   procedure Set_Offset(mem      : in out Offset_Type;
                        offset   : in Address_Type);

   overriding
   function Clone(mem : Offset_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Offset_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   overriding
   function To_String(mem : Offset_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Offset_Type) return Cost_Type;

private

   type Offset_Type is new Transform_Type with record
      offset : Address_Type;
   end record;

   overriding
   function Apply(mem      : Offset_Type;
                  address  : Address_Type) return Address_Type;


end Memory.Transform.Offset;
