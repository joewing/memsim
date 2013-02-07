
with Memory.Prefetch;

separate (Parser)
procedure Parse_Prefetch(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is
   mem         : Memory_Pointer := null;
   stride      : Address_Type := 1;
   multiplier  : Address_Type := 1;
begin
   while Get_Type(lexer) = Open loop
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Match(lexer, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(lexer, "memory set multiple times in prefetch");
            end if;
            Parse_Memory(lexer, mem);
         else
            declare
               value : constant String := Get_Value(lexer);
            begin
               Match(lexer, Literal);
               if name = "stride" then
                  stride := Address_Type'Value(value);
               elsif name = "multiplier" then
                  multiplier := Address_Type'Value(value);
               else
                  Raise_Error(lexer,
                              "invalid attribute in prefetch: " & name);
               end if;
            end;
         end if;
      end;
      Match(lexer, Close);
   end loop;
   if mem = null then
      Raise_Error(lexer, "memory not set in prefetch");
   end if;
   result := Memory_Pointer(Prefetch.Create_Prefetch(mem, stride,
                                                     multiplier));
exception
   when Data_Error =>
      Destroy(mem);
      Raise_Error(lexer, "invalid value in prefetch");
   when Parse_Error =>
      Destroy(mem);
      raise Parse_Error;
end Parse_Prefetch;
