
with Memory.Option; use Memory.Option;
with Util; use Util;

separate (Parser)
procedure Parse_Option(parser : in out Parser_Type;
                       result : out Memory_Pointer) is

   mem : Option_Pointer := Create_Option;

begin

   while Get_Type(parser) = Open loop
      declare
         other : Memory_Pointer := null;
      begin
         Parse_Memory(parser, other);
         Add_Memory(mem.all, other);
      end;
   end loop;
   result := Memory_Pointer(mem);

exception
   when Parse_Error =>
      Destroy(Memory_Pointer(mem));
      raise Parse_Error;

end Parse_Option;
