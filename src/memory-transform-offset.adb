
package body Memory.Transform.Offset is

   function Create_Offset(mem    : Memory_Pointer;
                          offset : Integer) return Offset_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      Set_Memory(result.all, mem);
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

   function To_String(mem : Offset_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(transform ");
      Append(result, "(offset" & Address_Type'Image(mem.offset) & ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Offset_Type) return Cost_Type is
   begin
      return 28 * Address_Type'Size;
   end Get_Cost;

end Memory.Transform.Offset;
