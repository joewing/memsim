
package Memory.Transform.Shift is

   type Shift_Type is new Transform_Type with private;

   type Shift_Pointer is access all Shift_Type'Class;

   function Create_Shift(mem     : Memory_Pointer;
                         shift   : Integer) return Shift_Pointer;

private

   type Shift_Type is new Transform_Type with record
      shift : Natural;
   end record;

   overriding
   function Apply(mem      : Shift_Type;
                  address  : Address_Type) return Address_Type;

end Memory.Transform.Shift;
