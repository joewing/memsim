
with Memory.Flash;
with Util; use Util;

separate (Parser)
procedure Parse_Flash(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is

   word_size      : Positive := 8;
   block_size     : Positive := 256;
   read_latency   : Time_Type := 10;
   write_latency  : Time_Type := 1000;

begin

   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         declare
            value : constant String := Get_Value(parser);
         begin
            Match(parser, Literal);
            if name = "word_size" then
               word_size := Positive'Value(value);
            elsif name = "block_size" then
               block_size := Positive'Value(value);
            elsif name = "read_latency" then
               read_latency := Time_Type'Value(value);
            elsif name = "write_latency" then
               write_latency := Time_Type'Value(value);
            else
               Raise_Error(parser, "invalid attribute in flash: " & name);
            end if;
         end;
      end;
      Match(parser, Close);
   end loop;
   result := Memory_Pointer(Flash.Create_Flash(word_size, block_size,
                                               read_latency, write_latency));
exception
   when Data_Error | Constraint_Error =>
      Raise_Error(parser, "invalid value in flash");
end Parse_Flash;
