
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Parser.Transform_Parser;

separate (Parser)
procedure Parse_Offset(parser : in out Parser_Type;
                       result : out Memory_Pointer) is

   package Offset_Parser is new Transform_Parser(
      T_Type            => Memory.Transform.Offset.Offset_Type,
      T_Pointer         => Memory.Transform.Offset.Offset_Pointer,
      Create_Transform  => Memory.Transform.Offset.Create_Offset
   );

begin

   Offset_Parser.Parse(parser, result);

end Parse_Offset;
