
with Memory.SPM;

separate (Parser)
procedure Parse_SPM(lexer  : in out Lexer_Type;
                    result : out Memory_Pointer) is

   mem      : Memory_Pointer := null;
   size     : Natural := 0;
   latency  : Time_Type := 1;

begin
   while Get_Type(lexer) = Open loop
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Match(lexer, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(lexer, "memory declared multiple times in spm");
            end if;
            Parse_Memory(lexer, mem);
         else
            declare
               value : constant String := Get_Value(lexer);
            begin
               Match(lexer, Literal);
               if name = "size" then
                  size := Natural'Value(value);
               elsif name = "latency" then
                  latency := Time_Type'Value(value);
               else
                  Raise_Error(lexer, "invalid spm attribute: " & name);
               end if;
            end;
         end if;
      end;
      Match(lexer, Close);
   end loop;
   if mem = null then
      Raise_Error(lexer, "no memory specified in spm");
   end if;
   result := Memory_Pointer(SPM.Create_SPM(mem, size, latency));
exception
   when Data_Error =>
      Raise_Error(lexer, "invalid value in spm");
end Parse_SPM;
