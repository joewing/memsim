
with Memory.Dup;

separate (Parser)
procedure Parse_Dup(parser  : in out Parser_Type;
                    result : out Memory_Pointer) is

   dp    : Dup.Dup_Pointer := Dup.Create_Dup;
   mem   : Memory_Pointer := null;

begin
   while Get_Type(parser) = Open loop
      Parse_Memory(parser, mem);
      Dup.Add_Memory(dp.all, mem);
   end loop;
   result := Memory_Pointer(dp);
exception
   when others =>
      Destroy(Memory_Pointer(dp));
      raise Parse_Error;
end Parse_Dup;
