

with Memory.DRAM; use type Memory.DRAM.DRAM_Pointer;
with Util;        use Util;

separate (Parser)
procedure Parse_DRAM(parser   : in out Parser_Type;
                     result   : out Memory_Pointer) is

   ptr         : DRAM.DRAM_Pointer := null;

   cas_cycles  : Time_Type := 5;
   rcd_cycles  : Time_Type := 5;
   rp_cycles   : Time_Type := 5;
   wb_cycles   : Time_Type := 0;
   word_size   : Positive  := 8;
   page_size   : Positive  := 1024;
   page_count  : Positive  := 16384;
   width       : Positive  := 2;
   burst_size  : Positive  := 1;
   multiplier  : Time_Type := 1;
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
            elsif name = "wb_cycles" then
               wb_cycles := Time_Type'Value(value);
            elsif name = "word_size" then
               word_size := Positive'Value(value);
            elsif name = "page_size" then
               page_size := Positive'Value(value);
            elsif name = "page_count" then
               page_count := Positive'Value(value);
            elsif name = "width" then
               width := Positive'Value(value);
            elsif name = "multiplier" then
               multiplier := Time_Type'Value(value);
            elsif name = "open_page" then
               open_page := Parse_Boolean(value);
            elsif name = "burst_size" then
               burst_size := Positive'Value(value);
            else
               Raise_Error(parser, "invalid dram attribute: " & name);
            end if;
         end;
      end;
      Match(parser, Close);
   end loop;

   ptr := DRAM.Create_DRAM(cas_cycles,
                           rcd_cycles,
                           rp_cycles,
                           wb_cycles,
                           multiplier,
                           word_size,
                           page_size,
                           page_count,
                           width,
                           burst_size,
                           open_page);
   if ptr = null then
      Raise_Error(parser, "invalid dram configuration");
   end if;

   result := Memory_Pointer(ptr);

exception

   when Data_Error | Constraint_Error =>
      Raise_Error(parser, "invalid value in dram");

end Parse_DRAM;
