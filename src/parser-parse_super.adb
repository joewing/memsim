
with Memory.Super_Time;
with Memory.Super_Writes;
with Memory.Super_None;
with Util; use Util;

separate (Parser)
procedure Parse_Super(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is

   type Opt_Type is (Opt_Time, Opt_Writes, Opt_None);

   max_cost       : Cost_Type := 1e6;
   permute_only   : Boolean := False;
   dram           : Memory_Pointer := null;
   seed           : Integer := 0;
   max_iterations : Long_Integer := 1000;
   opt            : Opt_Type := Opt_Time;
   sp             : Memory_Pointer := null;

begin

   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "memory" then
            if dram = null then
               Parse_Memory(parser, dram);
            else
               Destroy(dram);
               Raise_Error(parser, "duplicate memory in super");
            end if;
         else
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "max_cost" then
                  max_cost := Cost_Type'Value(value);
               elsif name = "seed" then
                  seed := Integer'Value(value);
               elsif name = "max_iterations" then
                  max_iterations := Long_Integer'Value(value);
               elsif name = "optimize" then
                  if value = "time" then
                     opt := Opt_Time;
                  elsif value = "writes" then
                     opt := Opt_Writes;
                  elsif value = "none" then
                     opt := Opt_None;
                  else
                     Raise_Error(parser, "invalid optimization target: " &
                                 value);
                  end if;
               elsif name = "permute_only" then
                  if value = "true" then
                     permute_only := True;
                  elsif value = "false" then
                     permute_only := False;
                  else
                     Raise_Error(parser, "invalid permute_only setting; " &
                                 value);
                  end if;
               else
                  Raise_Error(parser, "invalid attribute in super: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;

   case opt is
      when Opt_Time =>
         sp := Memory_Pointer(Super_Time.Create_Super(dram, max_cost, seed,
                                                      max_iterations,
                                                      permute_only));
      when Opt_Writes =>
         sp := Memory_Pointer(Super_Writes.Create_Super(dram, max_cost, seed,
                                                        max_iterations,
                                                        permute_only));
      when Opt_None =>
         sp := Memory_Pointer(Super_None.Create_Super(dram, max_cost, seed,
                                                      max_iterations,
                                                      permute_only));
   end case;

   if sp = null then
      Raise_Error(parser, "invalid initial memory");
   else
      result := sp;
   end if;

exception
   when Data_Error | Constraint_Error =>
      if dram /= null then
         Destroy(dram);
      end if;
      Raise_Error(parser, "invalid value in super");
end Parse_Super;
