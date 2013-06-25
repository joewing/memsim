
with Memory.Split;      use Memory.Split;
with Memory.Container;  use Memory.Container;
with Util;              use Util;

separate (Parser)
procedure Parse_Split(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is
   split    : Split_Pointer := Create_Split;
   mem      : Memory_Pointer  := null;
   bank0    : Memory_Pointer  := null;
   bank1    : Memory_Pointer  := null;
   offset   : Address_Type    := 0;
begin
   Push_Wrapper(parser, Wrapper_Pointer(split), 2);
   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "bank0" then
            if bank0 = null then
               Parse_Memory(parser, bank0);
            else
               Destroy(bank0);
               Raise_Error(parser, "duplicate bank0 in split");
            end if;
         elsif name = "bank1" then
            if bank1 = null then
               Parse_Memory(parser, bank1);
            else
               Destroy(bank1);
               Raise_Error(parser, "duplicate bank1 in split");
            end if;
         elsif name = "memory" then
            if mem = null then
               Parse_Memory(parser, mem);
            else
               Destroy(mem);
               Raise_Error(parser, "duplicate memory in split");
            end if;
         else
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "offset" then
                  offset := Address_Type'Value(value);
               else
                  Raise_Error(parser,
                              "invalid attribute in split: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;
   if bank0 = null then
      Raise_Error(parser, "bank0 not specified in split");
   elsif bank1 = null then
      Raise_Error(parser, "bank1 not specified in split");
   elsif mem = null then
      Raise_Error(parser, "memory not specified in split");
   end if;
   Set_Bank(split.all, 0, bank0);
   Set_Bank(split.all, 1, bank1);
   Set_Memory(split.all, mem);
   Set_Offset(split.all, offset);
   result := Memory_Pointer(split);
exception
   when Data_Error | Constraint_Error =>
      Destroy(Memory_Pointer(split));
      Delete_Wrapper(parser);
      Raise_Error(parser, "invalid value in split");
end Parse_Split;
