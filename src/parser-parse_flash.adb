
with Memory.Flash;

separate (Parser)
procedure Parse_Flash(lexer   : in out Lexer_Type;
                      result  : out Memory_Pointer) is

   word_size      : Positive := 8;
   block_size     : Positive := 256;
   read_latency   : Time_Type := 10;
   write_latency  : Time_Type := 1000;

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
            if name = "word_size" then
               word_size := Positive'Value(value);
            elsif name = "block_size" then
               block_size := Positive'Value(value);
            elsif name = "read_latency" then
               read_latency := Time_Type'Value(value);
            elsif name = "write_latency" then
               write_latency := Time_Type'Value(value);
            else
               Raise_Error(lexer, "invalid attribute in flash: " & name);
            end if;
         end;
      end;
      Match(lexer, Close);
   end loop;
   result := Memory_Pointer(Flash.Create_Flash(word_size, block_size,
                                               read_latency, write_latency));
exception
   when Data_Error =>
      Raise_Error(lexer, "invalid value in flash");
end Parse_Flash;
