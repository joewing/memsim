
with Memory.Trace;

separate (Parser)
procedure Parse_Trace(lexer   : in out Lexer_Type;
                      result  : out Memory_Pointer) is

   mem : Memory_Pointer := null;

begin

   if Get_Type(lexer) = Open then
      Parse_Memory(lexer, mem);
   end if;

   result := Memory_Pointer(Trace.Create_Trace(mem));

end Parse_Trace;
