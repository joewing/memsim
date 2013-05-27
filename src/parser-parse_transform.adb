
with Memory.Transform;
with Memory.Transform.Offset;
with Memory.Transform.Shift;

separate (Parser)
procedure Parse_Transform(parser : in out Parser_Type;
                          result : out Memory_Pointer) is

   type Operator_Type is (Op_Invalid, Op_Offset, Op_Shift);

   mem         : Memory_Pointer := null;
   op          : Operator_Type := Op_Invalid;
   offset      : Integer := 0;
   shift       : Integer := 0;
   word_size   : Natural := 8;

begin

   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(parser,
                           "memory declared multiple times in transform");
            end if;
            Parse_Memory(parser, mem);
         else
            if (name = "offset" or name = "shift") and op /= Op_Invalid then
               Raise_Error(parser,
                           "multiple operations specified in transform");
            end if;
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "offset" then
                  offset := Integer'Value(value);
                  op := Op_Offset;
               elsif name = "shift" then
                  shift := Integer'Value(value);
                  op := Op_Shift;
               elsif name = "word_size" then
                  word_size := Natural'Value(value);
               else
                  Raise_Error(parser, "invalid transform attribute: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;

   if mem = null then
      Raise_Error(parser, "no memory specified in transform");
   end if;
   case op is
      when Op_Offset =>
         result := Memory_Pointer(Transform.Offset.Create_Offset(mem, offset));
      when Op_Shift =>
         result := Memory_Pointer(Transform.Shift.Create_Shift(mem,
                                                               word_size,
                                                               shift));
      when Op_Invalid =>
         Raise_Error(parser, "no operator specified in transform");
   end case;

exception

   when Data_Error | Constraint_Error =>
      Raise_Error(parser, "invalid value in transform");

end Parse_Transform;
