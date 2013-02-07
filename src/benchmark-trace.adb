
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Streams.Stream_IO;      use Ada.Streams;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

package body Benchmark.Trace is

   type Access_Type is (Read, Write, Idle);

   type Memory_Access is record
      t     : Access_Type;
      value : Address_Type;
      size  : Natural;
   end record;

   Buffer_Size : constant := 2 ** 22;

   type Stream_Data is record
      file     : Stream_IO.File_Type;
      buffer   : Stream_Element_Array(1 .. Buffer_Size);
      pos      : Stream_Element_Offset := Stream_Element_Offset'Last;
      last     : Stream_Element_Offset := Stream_Element_Offset'First;
      total    : Long_Integer := 0;
   end record;

   type Stream_Data_Pointer is access Stream_Data;

   procedure Destroy is new Ada.Unchecked_Deallocation(Stream_Data,
                                                       Stream_Data_Pointer);

   task type Consumer_Task is
      entry Initialize(m : in Memory_Pointer;
                       s : in Time_Type);
      entry Process(data : in Memory_Access);
   end Consumer_Task;

   task body Consumer_Task is
      mem         : Memory_Pointer;
      spacing     : Time_Type;
   begin
      accept Initialize(m : in Memory_Pointer;
                        s : in Time_Type) do
         mem := m;
         spacing := s;
      end Initialize;
      loop
         select
            accept Process(data : in Memory_Access) do
               case data.t is
                  when Read   =>
                     Read(mem.all, data.value, data.size);
                  when Write  =>
                     Write(mem.all, data.value, data.size);
                  when Idle   =>
                     Idle(mem.all, Time_Type(data.value));
               end case;
               Idle(mem.all, spacing);
            end Process;
         or
            terminate;
         end select;
      end loop;
   end Consumer_Task;

   function Create_Trace return Benchmark_Pointer is
   begin
      return new Trace_Type;
   end Create_Trace;

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

   procedure Get_Character(sdata : in Stream_Data_Pointer;
                           ch    : out Character) is
   begin
      if sdata.pos > sdata.last then
         Stream_IO.Read(sdata.file, sdata.buffer, sdata.last);
         if sdata.last < sdata.buffer'First then
            raise Stream_IO.End_Error;
         end if;
         sdata.total := sdata.total
                      + Long_Integer(sdata.last - sdata.buffer'First + 1);
         Put_Line("Read" & Long_Integer'Image(sdata.total) & " bytes");
         sdata.pos := sdata.buffer'First;
      end if;
      ch := Character'Val(sdata.buffer(sdata.pos));
      sdata.pos := sdata.pos + 1;
   end Get_Character;

   procedure Read_Access(sdata   : in Stream_Data_Pointer;
                         result  : out Memory_Access) is
      ch : Character := '?';
   begin

      -- Determine if this is a read or a write.
      loop
         exit when ch = 'R' or ch = 'W' or ch = 'I';
         Get_Character(sdata, ch);
      end loop;
      case ch is
         when 'R'    => result.t := Read;
         when 'W'    => result.t := Write;
         when others => result.t := Idle;
      end case;

      -- Read the address/time.
      loop
         Get_Character(sdata, ch);
         result.value := To_Address(ch);
         exit when result.value < 16;
      end loop;
      loop
         Get_Character(sdata, ch);
         declare
            temp : constant Address_Type := To_Address(ch);
         begin
            exit when temp >= 16;
            result.value := result.value * 16 + temp;
         end;
      end loop;

      -- Read the size if a read or write.
      if result.t = Read or result.t = Write then
         result.size := 0;
         loop
            Get_Character(sdata, ch);
            declare
               temp : constant Natural := Natural(To_Address(ch));
            begin
               exit when temp >= 16;
               result.size := result.size * 16 + temp;
            end;
         end loop;
      else
         result.size := 1;
      end if;

   end Read_Access;

   procedure Set_Argument(benchmark : in out Trace_Type;
                          arg       : in String) is
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "file") then
         benchmark.file_name := To_Unbounded_String(value);
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in out Trace_Type) is
      consumer : Consumer_Task;
      sdata    : Stream_Data_Pointer := new Stream_Data;
      mdata    : Memory_Access;
   begin
      consumer.Initialize(benchmark.mem, benchmark.spacing);
      Stream_IO.Open(File => sdata.file,
                     Mode => Stream_IO.In_File,
                     Name => To_String(benchmark.file_name));
      loop
         Read_Access(sdata, mdata);
         consumer.Process(mdata);
      end loop;
   exception
      when Ada.Streams.Stream_IO.End_Error =>
         Stream_IO.Close(sdata.file);
         Destroy(sdata);
      when ex: others =>
         Put_Line("error: could not open " & To_String(benchmark.file_name) &
                  ": " & Exception_Message(ex));
         Stream_IO.Close(sdata.file);
         Destroy(sdata);
   end Run;

end Benchmark.Trace;

