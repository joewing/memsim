
package body Memory.Transform.Shift is

   function Create_Shift(mem     : Memory_Pointer;
                         shift   : Integer) return Shift_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      Set_Memory(result.all, mem);
      if shift < 0 then
         result.shift := Natural(Address_Type'Size + shift);
      else
         result.shift := Natural(shift);
      end if;
      return result;
   end Create_Shift;

   function Apply(mem      : Shift_Type;
                  address  : Address_Type) return Address_Type is
      right    : constant Address_Type := address * (2 ** mem.shift);
      lshift   : constant Natural := Address_Type'Size - mem.shift;
      left     : constant Address_Type := address / (2 ** lshift);
   begin
      return right or left;
   end Apply;

   function To_String(mem : Shift_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(transform ");
      Append(result, "(shift" & Natural'Image(mem.shift) & ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

end Memory.Transform.Shift;
