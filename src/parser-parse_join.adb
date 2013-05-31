
with Memory.Join;

separate (Parser)
procedure Parse_Join(parser   : in out Parser_Type;
                     result   : out Memory_Pointer) is
   wrapper  : Wrapper_Pointer;
   index    : Natural;
begin
   Pop_Wrapper(parser, wrapper, index);
   result := Memory_Pointer(Join.Create_Join(wrapper, index));
end Parse_Join;
