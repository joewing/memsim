
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Streams.Stream_IO;

package body Benchmark.Trace is

   type Access_Type is (Read, Write, Modify, Idle);

   type Memory_Access is record
      t     : Access_Type;
      value : Address_Type;
      size  : Natural;
   end record;

   type Parse_State_Type is (State_Action,
                             State_Pre_Address,
                             State_Address,
                             State_Pre_Size,
                             State_Size);

   procedure Parse_Action(benchmark : in out Trace_Type;
                          mdata     : in out Memory_Access;
                          state     : in out Parse_State_Type);

   function To_Address(ch : Character) return Address_Type is
      pos : constant Integer := Character'Pos(ch);
   begin
      if ch in '0' .. '9' then
         return Address_Type(pos - Character'Pos('0'));
      elsif ch in 'a' .. 'f' then
         return Address_Type(pos - Character'Pos('a') + 10);
      else
         return 16;
      end if;
   end To_Address;

   procedure Process_Action(benchmark  : in Trace_Type;
                            mdata      : in Memory_Access) is
   begin
      case mdata.t is
         when Read   =>
            Read(benchmark, mdata.value, mdata.size);
         when Write  =>
            Write(benchmark, mdata.value, mdata.size);
         when Modify =>
            Read(benchmark, mdata.value, mdata.size);
            Write(benchmark, mdata.value, mdata.size);
         when Idle   =>
            Idle(benchmark, Time_Type(mdata.value));
      end case;
      Idle(benchmark, benchmark.spacing);
   end Process_Action;

   procedure Parse_Action(benchmark : in out Trace_Type;
                          mdata     : in out Memory_Access;
                          state     : in out Parse_State_Type) is

      value : Address_Type;
      ch    : Character;

   begin
      while benchmark.last >= benchmark.pos loop
         ch := Character'Val(benchmark.buffer(benchmark.pos));
         case state is
            when State_Action =>
               state := State_Pre_Address;
               case ch is
                  when 'R'    => mdata.t := Read;
                  when 'W'    => mdata.t := Write;
                  when 'M'    => mdata.t := Modify;
                  when 'I'    => mdata.t := Idle;
                  when others => state := State_Action;
               end case;
               benchmark.pos := benchmark.pos + 1;
            when State_Pre_Address =>
               mdata.value := To_Address(ch);
               if mdata.value < 16 then
                  state := State_Address;
                  benchmark.pos := benchmark.pos + 1;
               else
                  state := State_Action;
               end if;
            when State_Address =>
               value := To_Address(ch);
               if value < 16 then
                  mdata.value := mdata.value * 16 + value;
                  benchmark.pos := benchmark.pos + 1;
               elsif mdata.t = Idle then
                  mdata.size := 1;
                  state := State_Action;
                  Process_Action(benchmark, mdata);
               else
                  state := State_Pre_Size;
                  benchmark.pos := benchmark.pos + 1;
               end if;
            when State_Pre_Size =>
               mdata.size := Natural(To_Address(ch));
               if mdata.size < 16 then
                  state := State_Size;
               end if;
               benchmark.pos := benchmark.pos + 1;
            when State_Size =>
               value := To_Address(ch);
               if value < 16 then
                  mdata.size := mdata.size * 16 + Natural(value);
                  benchmark.pos := benchmark.pos + 1;
               else
                  state := State_Action;
                  Process_Action(benchmark, mdata);
               end if;
         end case;
      end loop;
   end Parse_Action;

   function Create_Trace return Benchmark_Pointer is
      result : constant Trace_Pointer := new Trace_Type;
   begin
      result.spacing := 0;
      return Benchmark_Pointer(result);
   end Create_Trace;

   procedure Set_Argument(benchmark : in out Trace_Type;
                          arg       : in String) is
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "file") then
         benchmark.file_names.Append(To_Unbounded_String(value));
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Reset(benchmark : in out Trace_Type) is
   begin
      if benchmark.context = 0 then
         benchmark.context := Natural(benchmark.file_names.Length) - 1;
      else
         benchmark.context := benchmark.context - 1;
      end if;
      Reset(benchmark.mem.all, benchmark.context);
   end Reset;

   procedure Run(benchmark : in out Trace_Type) is
      name     : Unbounded_String;
      file     : Stream_IO.File_Type;
      mdata    : Memory_Access;
      state    : Parse_State_Type := State_Action;
   begin
      if benchmark.file_names.Length = 0 then
         benchmark.file_names.Append(To_Unbounded_String("trace.txt"));
      end if;
      name := benchmark.file_names.Element(benchmark.context);
      Stream_IO.Open(File => file,
                     Mode => Stream_IO.In_File,
                     Name => To_String(name));
      loop
         Stream_IO.Read(file, benchmark.buffer, benchmark.last);
         exit when benchmark.last < benchmark.buffer'First;
         benchmark.pos := benchmark.buffer'First;
         Parse_Action(benchmark, mdata, state);
      end loop;
      benchmark.buffer(benchmark.buffer'First) := 0;
      benchmark.pos := benchmark.buffer'First;
      benchmark.last := benchmark.pos;
      Parse_Action(benchmark, mdata, state);
      Stream_IO.Close(file);
   exception
      when ex: Device_Error =>
         Put_Line("error: could not read " & To_String(name) &
                  ": " & Exception_Name(ex) & ": " & Exception_Message(ex));
         Stream_IO.Close(file);
         raise;
      when others =>
         Stream_IO.Close(file);
         raise;
   end Run;

end Benchmark.Trace;
