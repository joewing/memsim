
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Sequential_IO;

package body Benchmark.Trace is

   package Character_IO is new Ada.Sequential_IO(Character);

   type Access_Type is (Read, Write, Idle);

   type Memory_Access is record
      t     : Access_Type;
      value : Address_Type;
   end record;

   task type Consumer_Task is
      entry Initialize(m : in Memory_Pointer;
                       s : in Time_Type);
      entry Process(data : in Memory_Access);
      entry Stop;
   end Consumer_Task;

   task body Consumer_Task is
      mem         : Memory_Pointer;
      spacing     : Time_Type;
      done        : Boolean := False;
   begin
      accept Initialize(m : in Memory_Pointer;
                        s : in Time_Type) do
         mem := m;
         spacing := s;
      end Initialize;
      while not done loop
         select
            accept Process(data : in Memory_Access) do
               case data.t is
                  when Read   =>
                     Read(mem.all, data.value);
                  when Write  =>
                     Write(mem.all, data.value);
                  when Idle   =>
                     Idle(mem.all, Time_Type(data.value));
               end case;
               Idle(mem.all, spacing);
            end Process;
         or
            accept Stop do
               done := True;
            end Stop;
         end select;
      end loop;
   end Consumer_Task;

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
         when 'R'    => result.t := Read;
         when 'W'    => result.t := Write;
         when others => result.t := Idle;
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
      file     : Character_IO.File_Type;
      consumer : Consumer_Task;
   begin
      consumer.Initialize(benchmark.mem, benchmark.spacing);
      Character_IO.Open(File => file,
                        Mode => Character_IO.In_File,
                        Name => To_String(benchmark.file_name));
      loop
         declare
            data : constant Memory_Access := Read_Access(file);
         begin
            consumer.Process(data);
         end;
      end loop;
   exception
      when Character_IO.Name_Error =>
         Put_Line("error: could not open " & To_String(benchmark.file_name));
         consumer.Stop;
      when Character_IO.End_Error =>
         Character_IO.Close(file);
         consumer.Stop;
   end Run;

end Benchmark.Trace;

