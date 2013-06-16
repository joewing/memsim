
with Memory.Transform.Shift;  use Memory.Transform.Shift;
with Parser.Transform_Parser;

separate (Parser)
procedure Parse_Shift(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is

   package Shift_Parser is new Transform_Parser(
      T_Type            => Memory.Transform.Shift.Shift_Type,
      T_Pointer         => Memory.Transform.Shift.Shift_Pointer,
      Create_Transform  => Memory.Transform.Shift.Create_Shift
   );

begin

   Shift_Parser.Parse(parser, result);

end Parse_Shift;
