
with Memory.Register;

separate (Parser)
procedure Parse_Register(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is

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
               Destroy(mem);
               Raise_Error(parser,
                           "memory declared multiple times in register");
            end if;
            Parse_Memory(parser, mem);
         else
            Raise_Error(parser, "invalid register attribute: " & name);
         end if;
      end;
      Match(parser, Close);
   end loop;

   if mem = null then
      Raise_Error(parser, "no memory specified in register");
   end if;

   result := Memory_Pointer(Register.Create_Register(mem));

end Parse_Register;
