
with Memory.Perfect_Prefetch;

separate (Parser)
procedure Parse_Perfect_Prefetch(parser   : in out Parser_Type;
                                 result   : out Memory_Pointer) is

   mem   : Memory_Pointer := null;

begin
   Parse_Memory(parser, mem);
   if mem = null then
      Raise_Error(parser, "memory not set in perfect_prefetch");
   end if;
   result := Memory_Pointer(Perfect_Prefetch.Create_Perfect_Prefetch(mem));
end Parse_Perfect_Prefetch;
