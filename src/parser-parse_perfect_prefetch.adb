
with Memory.Perfect_Prefetch;

separate (Parser)
procedure Parse_Perfect_Prefetch(lexer    : in out Lexer_Type;
                                 result   : out Memory_Pointer) is

   mem   : Memory_Pointer := null;

begin
   Parse_Memory(lexer, mem);
   if mem = null then
      Raise_Error(lexer, "memory not set in perfect_prefetch");
   end if;
   result := Memory_Pointer(Perfect_Prefetch.Create_Perfect_Prefetch(mem));
end Parse_Perfect_Prefetch;
