
with Memory.Dup;

separate (Parser)
procedure Parse_Dup(lexer  : in out Lexer_Type;
                    result : out Memory_Pointer) is

   dp    : Dup.Dup_Pointer := Dup.Create_Dup;
   mem   : Memory_Pointer := null;

begin
   while Get_Type(lexer) = Open loop
      Parse_Memory(lexer, mem);
      Dup.Add_Memory(dp.all, mem);
   end loop;
   result := Memory_Pointer(dp);
exception
   when others =>
      Destroy(Memory_Pointer(dp));
      raise Parse_Error;
end Parse_Dup;
