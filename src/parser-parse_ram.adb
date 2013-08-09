
with Memory.RAM;
with Util; use Util;

separate (Parser)
procedure Parse_RAM(parser : in out Parser_Type;
                    result : out Memory_Pointer) is

   latency     : Time_Type := 1;
   burst       : Time_Type := 0;
   word_size   : Positive  := 8;
   word_count  : Natural   := 0;

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
            if name = "latency" then
               latency := Time_Type'Value(value);
            elsif name = "burst" then
               burst := Time_Type'Value(value);
            elsif name = "word_size" then
               word_size := Positive'Value(value);
            elsif name = "word_count" then
               word_count := Natural'Value(value);
            else
               Raise_Error(parser, "invalid ram attribute: " & name);
            end if;
         end;
      end;
      Match(parser, Close);
   end loop;
   result := Memory_Pointer(RAM.Create_RAM(latency, burst,
                                           word_size, word_count));
exception
   when Data_Error | Constraint_Error =>
      Raise_Error(parser, "invalid value in ram");
end Parse_RAM;
