
with Memory.Bank;

separate (Parser)
procedure Parse_Bank(lexer    : in out Lexer_Type;
                     result   : out Memory_Pointer) is
   bank  : Memory.Bank.Bank_Pointer := Memory.Bank.Create_Bank;
   mem   : Memory_Pointer := null;
   key   : Address_Type := 0;
   mask  : Address_Type := 1;
begin
   while Get_Type(lexer) = Open loop
      Match(lexer, Open);
      mem   := null;
      key   := 0;
      mask  := 1;
      while Get_Type(lexer) = Open loop
         Match(lexer, Open);
         declare
            name : constant String := Get_Value(lexer);
         begin
            Match(lexer, Literal);
            if name = "memory" then
               if mem = null then
                  Parse_Memory(lexer, mem);
               else
                  Destroy(mem);
                  Raise_Error(lexer, "duplicate memories in bank");
               end if;
            else
               declare
                  value : constant String := Get_Value(lexer);
               begin
                  Match(lexer, Literal);
                  if name = "key" then
                     key := Address_Type'Value(value);
                  elsif name = "mask" then
                     mask := Address_Type'Value(value);
                  else
                     Raise_Error(lexer,
                                 "invalid attribute in bank: " & name);
                  end if;
               end;
            end if;
         end;
         Match(lexer, Close);
      end loop;
      Memory.Bank.Add_Bank(bank.all, mem, key, mask);
      Match(lexer, Close);
   end loop;
   result := Memory_Pointer(bank);
exception
   when Data_Error =>
      Destroy(Memory_Pointer(bank));
      Raise_Error(lexer, "invalid value in bank");
   when Parse_Error =>
      Destroy(Memory_Pointer(bank));
      raise Parse_Error;
end Parse_Bank;
