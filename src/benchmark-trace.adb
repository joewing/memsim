
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Sequential_IO;

package body Benchmark.Trace is

   package Character_IO is new Ada.Sequential_IO(Character);

   type Access_Type is (Read, Write, Idle);

   type Memory_Access is record
      t     : Access_Type;
      value : Address_Type;
   end record;

   function Create_Trace return Benchmark_Pointer is
   begin
      return new Trace_Type;
   end Create_Trace;

   function To_Address(ch : Character) return Address_Type is
   begin
      return Address_Type(Character'Pos(ch) - Character'Pos('0'));
   end To_Address;

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
      result.value := To_Address(ch);
      loop
         Character_IO.Read(file, ch);
         exit when ch < '0' or ch > '9';
         result.value := result.value * 10 + To_Address(ch);
      end loop;

      return result;
   end Read_Access;

   procedure Set_Argument(benchmark : in out Trace_Type;
                          arg       : in String) is
   begin
      if Check_Argument(arg, "file") then
         benchmark.file_name := To_Unbounded_String(Extract_Argument(arg));
      elsif Check_Argument(arg, "spacing") then
         benchmark.spacing := Time_Type'Value(Extract_Argument(arg));
      else
         raise Invalid_Argument;
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in out Trace_Type) is
      file : Character_IO.File_Type;
   begin
      Character_IO.Open(File => file,
                        Mode => Character_IO.In_File,
                        Name => To_String(benchmark.file_name));
      loop
         declare
            data : constant Memory_Access := Read_Access(file);
         begin
            case data.t is
               when Read   =>
                  Read(benchmark.mem.all, data.value);
               when Write  =>
                  Write(benchmark.mem.all, data.value);
               when Idle   =>
                  Idle(benchmark, Time_Type(data.value));
            end case;
         end;
         Idle(benchmark, benchmark.spacing);
      end loop;
   exception
      when Character_IO.Name_Error =>
         Put_Line("error: could not open " & To_String(benchmark.file_name));
      when Character_IO.End_Error =>
         Character_IO.Close(file);
   end Run;

end Benchmark.Trace;

