
with Memory.Join;

separate (Parser)
procedure Parse_Join(lexer    : in out Lexer_Type;
                     result   : out Memory_Pointer) is
begin
   result := Memory_Pointer(Join.Create_Join);
end Parse_Join;
