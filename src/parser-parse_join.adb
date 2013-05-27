
with Memory.Join;

separate (Parser)
procedure Parse_Join(parser   : in out Parser_Type;
                     result   : out Memory_Pointer) is
begin
   if parser.splits.Is_Empty then
      Raise_Error(parser, "join without split");
   end if;
   declare
      node  : Split_Node      := parser.splits.Last_Element;
      split : constant Split_Pointer   := node.split;
      index : constant Natural         := node.count;
   begin
      result := Memory_Pointer(Join.Create_Join(split, index));
      node.count := node.count + 1;
      if node.count = 2 then
         parser.splits.Delete_Last;
      else
         parser.splits.Replace_Element(parser.splits.Last, node);
      end if;
   end;
end Parse_Join;
