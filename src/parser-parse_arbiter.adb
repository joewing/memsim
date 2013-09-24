
with Memory.Arbiter;
with Util; use Util;

separate (Parser)
procedure Parse_Arbiter(parser : in out Parser_Type;
                        result : out Memory_Pointer) is

   mem : Memory_Pointer := null;

begin
   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(parser,
                           "memory declared multiple times in arbiter");
            end if;
            Parse_Memory(parser, mem);
         else
            Raise_Error(parser, "invalid arbiter attribute: " & name);
         end if;
      end;
      Match(parser, Close);
   end loop;

   if mem = null then
      Raise_Error(parser, "no memory specified in arbiter");
   end if;

   result := Memory_Pointer(Arbiter.Create_Arbiter(mem));

exception

   when Data_Error | Constraint_Error =>
      Raise_Error(parser, "invalid value in arbiter");

end Parse_Arbiter;
