
with Memory.Transform.Flip;   use Memory.Transform.Flip;
with Parser.Transform_Parser;

separate (Parser)
procedure Parse_Flip(parser : in out Parser_Type;
                     result : out Memory_Pointer) is

   package Flip_Parser is new Transform_Parser(
      T_Type            => Flip_Type,
      T_Pointer         => Flip_Pointer,
      Create_Transform  => Create_Flip
   );

begin

   Flip_Parser.Parse(parser, result);

end Parse_Flip;
