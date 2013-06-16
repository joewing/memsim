
with Memory.Transform.EOR;    use Memory.Transform.EOR;
with Parser.Transform_Parser;

separate (Parser)
procedure Parse_EOR(parser : in out Parser_Type;
                    result : out Memory_Pointer) is

   package EOR_Parser is new Transform_Parser(
      T_Type            => Memory.Transform.EOR.EOR_Type,
      T_Pointer         => Memory.Transform.EOR.EOR_Pointer,
      Create_Transform  => Memory.Transform.EOR.Create_EOR
   );

begin

   EOR_Parser.Parse(parser, result);

end Parse_EOR;
