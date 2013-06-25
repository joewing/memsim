
with Memory.SPM;
with Util; use Util;

separate (Parser)
procedure Parse_SPM(parser : in out Parser_Type;
                    result : out Memory_Pointer) is

   mem      : Memory_Pointer := null;
   size     : Natural := 0;
   latency  : Time_Type := 2;

begin
   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(parser, "memory declared multiple times in spm");
            end if;
            Parse_Memory(parser, mem);
         else
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "size" then
                  size := Natural'Value(value);
               elsif name = "latency" then
                  latency := Time_Type'Value(value);
               else
                  Raise_Error(parser, "invalid spm attribute: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;
   if mem = null then
      Raise_Error(parser, "no memory specified in spm");
   end if;
   result := Memory_Pointer(SPM.Create_SPM(mem, size, latency));
exception
   when Data_Error | Constraint_Error =>
      Raise_Error(parser, "invalid value in spm");
end Parse_SPM;
