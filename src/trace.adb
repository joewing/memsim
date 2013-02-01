
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Sequential_IO;

package body Trace is

   package Character_IO is new Ada.Sequential_IO(Character);

   type Access_Type is (Read, Write, Idle);

   type Memory_Access is record
      t     : Access_Type;
      value : Natural;
   end record;

   function To_Natural(ch : Character) return Natural is
   begin
      return Natural(Character'pos(ch) - Character'pos('0'));
   end To_Natural;

   function Read_Access(file : Character_IO.File_Type) return Memory_Access is
      result   : Memory_Access;
      ch       : Character;
   begin

      -- Determine if this is a read or a write.
      loop
         Character_IO.Read(file, ch);
         exit when ch = 'R' or ch = 'W' or ch = 'I';
      end loop;
      case ch is
         when 'R' | 'r' => result.t := Read;
         when 'W' | 'w' => result.t := Write;
         when others    => result.t := Idle;
      end case;

      -- Skip to the address.
      loop
         Character_IO.Read(file, ch);
         exit when ch >= '0' and ch <= '9';
      end loop;

      -- Read the value.
      result.value := To_Natural(ch);
      loop
         Character_IO.Read(file, ch);
         exit when ch < '0' or ch > '9';
         result.value := result.value * 10 + To_Natural(ch);
      end loop;

      return result;
   end Read_Access;

   procedure Process(mem      : in out Memory_Type'class;
                     name     : in String;
                     spacing  : in Time_Type := 1) is
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
               case data.t is
                  when Read   => Read(mem, Address_Type(data.value));
                  when Write  => Write(mem, Address_Type(data.value));
                  when Idle   => Idle(mem, Time_Type(data.value));
               end case;
            end;
            Idle(mem, spacing);
         end loop;
      exception
         when End_Error =>
            Character_IO.Close(file);
      end;
      Put_Line("Total time: " & Time_Type'image(Get_Time(mem)));
   end Process;

end Trace;

