
with Memory.Super_Time;
with Memory.Super_Writes;

separate (Parser)
procedure Parse_Super(lexer   : in out Lexer_Type;
                      result  : out Memory_Pointer) is

   type Opt_Type is (Opt_Time, Opt_Writes);

   max_cost    : Cost_Type := 1e6;
   dram        : Memory_Pointer := null;
   seed        : Integer := 0;
   opt         : Opt_Type := Opt_Time;

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
               if name = "max_cost" then
                  max_cost := Cost_Type'Value(value);
               elsif name = "seed" then
                  seed := Integer'Value(value);
               elsif name = "optimize" then
                  if value = "time" then
                     opt := Opt_Time;
                  elsif value = "writes" then
                     opt := Opt_Writes;
                  else
                     Raise_Error(lexer, "invalid optimization target: " &
                                 value);
                  end if;
               else
                  Raise_Error(lexer, "invalid attribute in super: " & name);
               end if;
            end;
         end if;
      end;
      Match(lexer, Close);
   end loop;
   case opt is
      when Opt_Time =>
         result := Memory_Pointer(Super_Time.Create_Super(dram, max_cost,
                                                          seed));
      when Opt_Writes =>
         result := Memory_Pointer(Super_Writes.Create_Super(dram, max_cost,
                                                            seed));
   end case;
exception
   when Data_Error | Constraint_Error =>
      if dram /= null then
         Destroy(dram);
      end if;
      Raise_Error(lexer, "invalid value in super");
end Parse_Super;
