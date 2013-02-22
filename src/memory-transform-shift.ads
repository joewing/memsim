
package Memory.Transform.Shift is

   type Shift_Type is new Transform_Type with private;

   type Shift_Pointer is access all Shift_Type'Class;

   function Create_Shift(mem     : Memory_Pointer;
                         shift   : Integer) return Shift_Pointer;

   function Random_Shift(generator  : RNG.Generator;
                         max_cost   : Cost_Type) return Memory_Pointer;

   function Get_Shift(mem : Shift_Type) return Natural;

   procedure Set_Shift(mem    : in out Shift_Type;
                       shift  : in Natural);

   overriding
   function Clone(mem : Shift_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Shift_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   overriding
   function To_String(mem : Shift_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Shift_Type) return Cost_Type;

private

   type Shift_Type is new Transform_Type with record
      shift : Natural;
   end record;

   overriding
   function Apply(mem      : Shift_Type;
                  address  : Address_Type) return Address_Type;

end Memory.Transform.Shift;
