

with Memory.DRAM;
with Util; use Util;

separate (Parser)
procedure Parse_DRAM(parser   : in out Parser_Type;
                     result   : out Memory_Pointer) is

   cas_cycles  : Time_Type := 5;
   rcd_cycles  : Time_Type := 5;
   rp_cycles   : Time_Type := 5;
   word_size   : Positive  := 8;
   page_words  : Positive  := 1024 / 8;
   row_count   : Positive  := 16384;
   open_page   : Boolean   := True;

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
            if name = "cas_cycles" then
               cas_cycles := Time_Type'Value(value);
            elsif name = "rcd_cycles" then
               rcd_cycles := Time_Type'Value(value);
            elsif name = "rp_cycles" then
               rp_cycles := Time_Type'Value(value);
            elsif name = "word_size" then
               word_size := Positive'Value(value);
            elsif name = "page_words" then
               page_words := Positive'Value(value);
            elsif name = "row_count" then
               row_count := Positive'Value(value);
            elsif name = "open_page" then
               open_page := Parse_Boolean(value);
            else
               Raise_Error(parser, "invalid dram attribute: " & name);
            end if;
         end;
      end;
      Match(parser, Close);
   end loop;

   result := Memory_Pointer(DRAM.Create_DRAM(cas_cycles,
                                             rcd_cycles,
                                             rp_cycles,
                                             word_size,
                                             page_words,
                                             row_count,
                                             open_page));

exception

   when Data_Error | Constraint_Error =>
      Raise_Error(parser, "invalid value in dram");

end Parse_DRAM;
