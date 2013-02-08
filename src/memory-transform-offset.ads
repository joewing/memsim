
package Memory.Transform.Offset is

   type Offset_Type is new Transform_Type with private;

   type Offset_Pointer is access all Offset_Type'Class;

   function Create_Offset(mem    : Memory_Pointer;
                          offset : Integer) return Offset_Pointer;

private

   type Offset_Type is new Transform_Type with record
      offset : Address_Type;
   end record;

   overriding
   function Apply(mem      : Offset_Type;
                  address  : Address_Type) return Address_Type;

end Memory.Transform.Offset;
