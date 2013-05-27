
with Memory.Trace;

separate (Parser)
procedure Parse_Trace(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is

   mem : Memory_Pointer := null;

begin

   if Get_Type(parser) = Open then
      Parse_Memory(parser, mem);
   end if;

   result := Memory_Pointer(Trace.Create_Trace(mem));

end Parse_Trace;
