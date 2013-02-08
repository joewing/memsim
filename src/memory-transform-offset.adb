
package body Memory.Transform.Offset is

   function Create_Offset(mem    : Memory_Pointer;
                          offset : Integer) return Offset_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      result.mem := mem;
      if offset < 0 then
         result.offset := 0 - Address_Type(-offset);
      else
         result.offset := Address_Type(offset);
      end if;
      return result;
   end Create_Offset;

   function Apply(mem      : Offset_Type;
                  address  : Address_Type) return Address_Type is
   begin
      return address + mem.offset;
   end Apply;

end Memory.Transform.Offset;
