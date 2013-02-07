
with Memory.RAM;

separate (Parser)
procedure Parse_RAM(lexer  : in out Lexer_Type;
                    result : out Memory_Pointer) is

   latency     : Time_Type := 1;
   word_size   : Positive  := 8;

begin
   while Get_Type(lexer) = Open loop
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Match(lexer, Literal);
         declare
            value : constant String := Get_Value(lexer);
         begin
            Match(lexer, Literal);
            if name = "latency" then
               latency := Time_Type'Value(value);
            elsif name = "word_size" then
               word_size := Positive'Value(value);
            else
               Raise_Error(lexer, "invalid ram attribute: " & value);
            end if;
         end;
      end;
      Match(lexer, Close);
   end loop;
   result := Memory_Pointer(RAM.Create_RAM(latency, word_size));
exception
   when Data_Error =>
      Raise_Error(lexer, "invalid value in ram");
end Parse_RAM;
