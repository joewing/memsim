
with Memory.Learn;

separate (Parser)
procedure Parse_Learn(lexer   : in out Lexer_Type;
                      result  : out Memory_Pointer) is
   mem : Memory_Pointer := null;
begin
   if Get_Type(lexer) = Open then
      Parse_Memory(lexer, mem);
   end if;
   result := Memory_Pointer(Learn.Create_Learn(mem));
end Parse_Learn;
