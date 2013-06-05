
with Memory.Container;        use Memory.Container;
with Memory.Transform.Flip;   use Memory.Transform.Flip;

separate (Parser)
procedure Parse_Flip(parser : in out Parser_Type;
                     result : out Memory_Pointer) is

   trans    : Flip_Pointer    := Create_Flip;
   mem      : Memory_Pointer  := null;
   bank     : Memory_Pointer  := null;

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
               Raise_Error(parser, "duplicate bank in flip");
            end if;
         elsif name = "memory" then
            if mem = null then
               Parse_Memory(parser, mem);
            else
               Destroy(mem);
               Raise_Error(parser, "duplicate memory in flip");
            end if;
         else
            Raise_Error(parser, "invalid attribute in flip: " & name);
         end if;
      end;
      Match(parser, Close);
   end loop;

   if mem = null then
      Raise_Error(parser, "memory not specified in flip");
   end if;
   Set_Memory(trans.all, mem);
   Set_Bank(trans.all, bank);

   result := Memory_Pointer(trans);

exception
   when others =>
      Destroy(Memory_Pointer(trans));
      raise;

end Parse_Flip;
