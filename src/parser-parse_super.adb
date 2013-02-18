
with Memory.Super;

separate (Parser)
procedure Parse_Super(lexer   : in out Lexer_Type;
                      result  : out Memory_Pointer) is

   sram_size   : Natural := 0;
   dram        : Memory_Pointer := null;

begin

   while Get_Type(lexer) = Open loop
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Match(lexer, Literal);
         if name = "memory" then
            if dram = null then
               Parse_Memory(lexer, dram);
            else
               Destroy(dram);
               Raise_Error(lexer, "duplicate memory in super");
            end if;
         else
            declare
               value : constant String := Get_Value(lexer);
            begin
               Match(lexer, Literal);
               if name = "sram_size" then
                  sram_size := Natural'Value(value);
               else
                  Raise_Error(lexer, "invalid attribute in super: " & name);
               end if;
            end;
         end if;
      end;
      Match(lexer, Close);
   end loop;
   result := Memory_Pointer(Super.Create_Super(sram_size, dram));
exception
   when Data_Error =>
      if dram /= null then
         Destroy(dram);
      end if;
      Raise_Error(lexer, "invalid value in super");
end Parse_Super;
