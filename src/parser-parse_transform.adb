
with Memory.Transform;
with Memory.Transform.Offset;

separate (Parser)
procedure Parse_Transform(lexer  : in out Lexer_Type;
                          result : out Memory_Pointer) is

   type Operator_Type is (Op_Invalid, Op_Offset);

   mem      : Memory_Pointer := null;
   op       : Operator_Type := Op_Invalid;
   offset   : Integer := 0;

begin

   while Get_Type(lexer) = Open loop
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Match(lexer, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(lexer,
                           "memory declared multiple times in transform");
            end if;
            Parse_Memory(lexer, mem);
         else
            if op /= Op_Invalid then
               Raise_Error(lexer,
                           "multiple operations specified in transform");
            end if;
            declare
               value : constant String := Get_Value(lexer);
            begin
               Match(lexer, Literal);
               if name = "offset" then
                  offset := Integer'Value(value);
                  op := Op_Offset;
               else
                  Raise_Error(lexer, "invalid transform attribute: " & name);
               end if;
            end;
         end if;
      end;
      Match(lexer, Close);
   end loop;

   if mem = null then
      Raise_Error(lexer, "no memory specified in transform");
   end if;
   case op is
      when Op_Offset =>
         result := Memory_Pointer(Transform.Offset.Create_Offset(mem, offset));
      when Op_Invalid =>
         Raise_Error(lexer, "no operator specified in transform");
   end case;

exception

   when Data_Error =>
      Raise_Error(lexer, "invalid value in transform");

end Parse_Transform;
