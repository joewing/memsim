
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Sequential_IO;

procedure FixTrace is

   package Character_IO is new Ada.Sequential_IO(Character);

   type Value_Type is mod 2 ** 64;

   input    : Character_IO.File_Type;
   min_addr : Value_Type   := Value_Type'Last;

   -- Extract the value from a hex digit.
   function Get_Value(ch : Character) return Value_Type is
   begin
      case ch is
         when '0' .. '9' =>
            return Character'Pos(ch) - Character'Pos('0');
         when 'a' .. 'f' =>
            return Character'Pos(ch) - Character'Pos('a') + 10;
         when others =>
            return 16;
      end case;
   end Get_Value;

   -- Display a value in hex.
   procedure Put_Value(value : in Value_Type) is
      hex      : constant String(1 .. 16) := "0123456789abcdef";
      buffer   : String(1 .. 64);
      index    : Natural := buffer'Last;
      temp     : Value_Type := value;
   begin
      while temp > 0 loop
         buffer(index) := hex(Integer(temp mod 16) + 1);
         temp  := temp / 16;
         index := index - 1;
      end loop;
      if index /= buffer'Last then
         Put(buffer(index + 1 .. buffer'Last));
      else
         Put("0");
      end if;
   end Put_Value;

   -- Compute the minimum address in the input file.
   procedure Compute_Min_Address is
      ch    : Character;
      addr  : Value_Type;
   begin
      Character_IO.Read(input, ch);
      while not Character_IO.End_Of_File(input) loop
         if ch = 'R' or ch = 'W' or ch = 'M' then
            Character_IO.Read(input, ch);
            if Get_Value(ch) < 16 then
               addr := Get_Value(ch);
               loop
                  Character_IO.Read(input, ch);
                  exit when ch = ':';
                  addr := addr * 16 + Get_Value(ch);
               end loop;
               if addr < min_addr then
                  min_addr := addr;
               end if;
               loop
                  Character_IO.Read(input, ch);
                  exit when Get_Value(ch) > 15;
               end loop;
            end if;
         else
            Character_IO.Read(input, ch);
         end if;
      end loop;
   exception
      when End_Error => null;
   end Compute_Min_Address;

   -- Output an updated address trace.
   procedure Update_Addresses is
      ch    : Character;
      addr  : Value_Type;
   begin
      Character_IO.Read(input, ch);
      while not Character_IO.End_Of_File(input) loop
         if ch = 'R' or ch = 'W' or ch = 'M' then
            Put(ch);
            Character_IO.Read(input, ch);
            if Get_Value(ch) < 16 then
               addr := Get_Value(ch);
               loop
                  Character_IO.Read(input, ch);
                  exit when ch = ':';
                  addr := addr * 16 + Get_Value(ch);
               end loop;
               Put_Value(addr - min_addr);
               Put(':');
               loop
                  Character_IO.Read(input, ch);
                  exit when Get_Value(ch) > 15;
                  Put(ch);
               end loop;
            end if;
         else
            Put(ch);
            Character_IO.Read(input, ch);
         end if;
      end loop;
   exception
      when End_Error => null;
   end Update_Addresses;

begin

   if Argument_Count /= 1 then
      Put_Line("usage: " & Command_Name & " <trace>");
      return;
   end if;

   Character_IO.Open(input, Character_IO.In_File, Argument(1));
   Compute_Min_Address;
   Character_IO.Reset(input);
   Update_Addresses;
   Character_IO.Close(input);

exception
   when ex: others =>
      Put_Line("ERROR: could not convert trace: " & Exception_Name(ex));

end FixTrace;
