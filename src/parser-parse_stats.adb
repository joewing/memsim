
with Memory.Stats;

separate (Parser)
procedure Parse_Stats(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is

   mem : Memory_Pointer := null;

begin
   if Get_Type(parser) = Open then
      Parse_Memory(parser, mem);
   end if;
   result := Memory_Pointer(Stats.Create_Stats(mem));
end Parse_Stats;
