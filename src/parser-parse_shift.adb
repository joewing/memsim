
with Memory.Container;        use Memory.Container;
with Memory.Transform.Shift;  use Memory.Transform.Shift;

separate (Parser)
procedure Parse_Shift(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is

   trans    : Shift_Pointer   := Create_Shift;
   mem      : Memory_Pointer  := null;
   bank     : Memory_Pointer  := null;
   shift    : Integer         := 0;

begin

   Push_Wrapper(parser, Wrapper_Pointer(trans), 1);

   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "bank" then
            if bank = null then
               Parse_Memory(parser, bank);
            else
               Destroy(bank);
               Raise_Error(parser, "duplicate bank in shift");
            end if;
         elsif name = "memory" then
            if mem = null then
               Parse_Memory(parser, mem);
            else
               Destroy(mem);
               Raise_Error(parser, "duplicate memory in shift");
            end if;
         else
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "value" then
                  shift := Integer'Value(value);
               else
                  Raise_Error(parser, "invalid attribute in shift: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;

   if mem = null then
      Raise_Error(parser, "memory not specified in shift");
   end if;
   Set_Memory(trans.all, mem);
   Set_Bank(trans.all, bank);

   Set_Shift(trans.all, shift);

   result := Memory_Pointer(trans);

exception

   when Data_Error | Constraint_Error =>
      Destroy(Memory_Pointer(trans));
      Delete_Wrapper(parser);
      Raise_Error(parser, "invalid value in shift");

end Parse_Shift;
