
with Memory.Container;        use Memory.Container;
with Memory.Transform.Offset; use Memory.Transform.Offset;

separate (Parser)
procedure Parse_Offset(parser : in out Parser_Type;
                       result : out Memory_Pointer) is

   trans    : Offset_Pointer  := Create_Offset;
   mem      : Memory_Pointer  := null;
   bank     : Memory_Pointer  := null;
   offset   : Integer         := 0;

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
               Raise_Error(parser, "duplicate bank in offset");
            end if;
         elsif name = "memory" then
            if mem = null then
               Parse_Memory(parser, mem);
            else
               Destroy(mem);
               Raise_Error(parser, "duplicate memory in offset");
            end if;
         else
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "value" then
                  offset := Integer'Value(value);
               else
                  Raise_Error(parser, "invalid attribute in offset: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;

   if mem = null then
      Raise_Error(parser, "memory not specified in offset");
   end if;
   Set_Memory(trans.all, mem);

   if bank = null then
      Raise_Error(parser, "bank not specified in offset");
   end if;
   Set_Bank(trans.all, bank);

   if offset < 0 then
      Set_Offset(trans.all, 0 - Address_Type(-offset));
   else
      Set_Offset(trans.all, Address_Type(offset));
   end if;

   result := Memory_Pointer(trans);

exception

   when Data_Error | Constraint_Error =>
      Destroy(Memory_Pointer(trans));
      Delete_Wrapper(parser);
      Raise_Error(parser, "invalid value in offset");

end Parse_Offset;
