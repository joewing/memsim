
with Memory.Container; use Memory.Container;
with Ada.Text_IO; use Ada.Text_IO;

package body Parser.Transform_Parser is

   procedure Parse(parser : in out Parser_Type;
                   result : out Memory_Pointer) is

      trans    : T_Pointer       := Create_Transform;
      tname    : constant String := Get_Name(trans.all);
      mem      : Memory_Pointer  := null;
      bank     : Memory_Pointer  := null;
      ivalue   : Long_Integer    := 0;

   begin

      Push_Wrapper(parser, Wrapper_Pointer(trans), 1);

      while Get_Type(parser) = Open loop
         Match(parser, Open);
         declare
            name : constant String := Get_Value(parser);
         begin
            Match(parser, Literal);
            if name = "bank" then
               if bank = null then
                  Parse_Memory(parser, bank);
               else
                  Destroy(bank);
                  Raise_Error(parser, "duplicate bank in " & tname);
               end if;
            elsif name = "memory" then
               if mem = null then
                  Parse_Memory(parser, mem);
               else
                  Destroy(mem);
                  Raise_Error(parser, "duplicate memory in " & tname);
               end if;
            else
               declare
                  value : constant String := Get_Value(parser);
               begin
                  Match(parser, Literal);
                  if name = "value" then
                     ivalue := Long_Integer'Value(value);
                  else
                     Raise_Error(parser, "invalid attribute in " &
                                 tname & ": " & name);
                  end if;
               end;
            end if;
         end;
         Match(parser, Close);
      end loop;

      if mem = null then
         Raise_Error(parser, "memory not specified in " & tname);
      end if;
      Set_Memory(Container_Type'Class(trans.all), mem);
      Set_Bank(trans.all, bank);

      Set_Value(trans.all, ivalue);

      result := Memory_Pointer(trans);

   exception

      when Data_Error | Constraint_Error =>
         Destroy(Memory_Pointer(trans));
         Delete_Wrapper(parser);
         Raise_Error(parser, "invalid value in " & tname);

   end Parse;

end Parser.Transform_Parser;
