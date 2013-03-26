
with Memory.Split;

separate (Parser)
procedure Parse_Split(lexer   : in out Lexer_Type;
                      result  : out Memory_Pointer) is
   mem      : Memory_Pointer  := null;
   bank0    : Memory_Pointer  := null;
   bank1    : Memory_Pointer  := null;
   offset   : Address_Type    := 0;
begin
   while Get_Type(lexer) = Open loop
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Match(lexer, Literal);
         if name = "bank0" then
            if bank0 = null then
               Parse_Memory(lexer, bank0);
            else
               Destroy(bank0);
               Raise_Error(lexer, "duplicate bank0 in split");
            end if;
         elsif name = "bank1" then
            if bank1 = null then
               Parse_Memory(lexer, bank1);
            else
               Destroy(bank1);
               Raise_Error(lexer, "duplicate bank1 in split");
            end if;
         elsif name = "memory" then
            if mem = null then
               Parse_Memory(lexer, mem);
            else
               Destroy(mem);
               Raise_Error(lexer, "duplicate memory in split");
            end if;
         else
            declare
               value : constant String := Get_Value(lexer);
            begin
               Match(lexer, Literal);
               if name = "offset" then
                  offset := Address_Type'Value(value);
               else
                  Raise_Error(lexer,
                              "invalid attribute in split: " & name);
               end if;
            end;
         end if;
      end;
      Match(lexer, Close);
   end loop;
   if bank0 = null then
      Raise_Error(lexer, "bank0 not specified in split");
   elsif bank1 = null then
      Raise_Error(lexer, "bank1 not specified in split");
   elsif mem = null then
      Raise_Error(lexer, "memory not specified in split");
   end if;
   result := Memory_Pointer(Memory.Split.Create_Split(mem, bank0, bank1,
                                                      offset));
exception
   when Data_Error | Constraint_Error =>
      Raise_Error(lexer, "invalid value in split");
end Parse_Split;
