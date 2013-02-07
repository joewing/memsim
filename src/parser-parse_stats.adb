
with Memory.Stats;

separate (Parser)
procedure Parse_Stats(lexer   : in out Lexer_Type;
                      result  : out Memory_Pointer) is

   mem : Memory_Pointer := null;

begin
   if Get_Type(lexer) = Open then
      Parse_Memory(lexer, mem);
   end if;
   result := Memory_Pointer(Stats.Create_Stats(mem));
end Parse_Stats;
