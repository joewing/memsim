
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;

package body Trace is

   package Character_IO is new Ada.Sequential_IO(Character);

   type Memory_Access is record
      is_read     : Boolean;
      address     : Address_Type;
   end record;

   function To_Address(ch : Character) return Address_Type is
   begin
      return Address_Type(Character'pos(ch) - Character'pos('0'));
   end To_Address;

   function Read_Access(file : Character_IO.File_Type) return Memory_Access is
      result   : Memory_Access;
      ch       : Character;
   begin

      -- Determine if this is a read or a write.
      loop
         Character_IO.Read(file, ch);
         exit when ch = 'R' or ch = 'W';
      end loop;
      result.is_read := ch = 'R';

      -- Skip to the address.
      loop
         Character_IO.Read(file, ch);
         exit when ch >= '0' and ch <= '9';
      end loop;

      -- Read the address.
      result.address := To_Address(ch);
      loop
         Character_IO.Read(file, ch);
         exit when ch < '0' or ch > '9';
         result.address := result.address * 10 + To_Address(ch);
      end loop;

      return result;
   end Read_Access;

   procedure Process(mem   : in out Memory_Type'class;
                     name  : String) is
      file : Character_IO.File_Type;
   begin
      Character_IO.Open(File => file,
                        Mode => Character_IO.In_File,
                        Name => name);
      begin
         loop
            declare
               data : constant Memory_Access := Read_Access(file);
            begin
               if data.is_read then
                  Read(mem, data.address);
               else
                  Write(mem, data.address);
               end if;
            end;
         end loop;
      exception
         when End_Error =>
            Character_IO.Close(file);
      end;
   end Process;

end Trace;

