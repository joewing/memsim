
with Memory.Prefetch;
with Util; use Util;

separate (Parser)
procedure Parse_Prefetch(parser   : in out Parser_Type;
                         result  : out Memory_Pointer) is
   mem         : Memory_Pointer := null;
   stride      : Address_Type := 1;
begin
   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(parser, "memory set multiple times in prefetch");
            end if;
            Parse_Memory(parser, mem);
         else
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "stride" then
                  stride := Address_Type'Value(value);
               else
                  Raise_Error(parser,
                              "invalid attribute in prefetch: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;
   if mem = null then
      Raise_Error(parser, "memory not set in prefetch");
   end if;
   result := Memory_Pointer(Prefetch.Create_Prefetch(mem, stride));
exception
   when Data_Error | Constraint_Error =>
      Destroy(mem);
      Raise_Error(parser, "invalid value in prefetch");
   when Parse_Error =>
      Destroy(mem);
      raise Parse_Error;
end Parse_Prefetch;
