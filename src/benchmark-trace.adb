
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

package body Benchmark.Trace is

   type Byte_Count_Type is mod 2 ** 64;

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

   procedure Destroy is new Ada.Unchecked_Deallocation(Stream_Buffer_Type,
                                                       Stream_Buffer_Pointer);

   procedure Destroy is new Ada.Unchecked_Deallocation(Buffer_Pool_Type,
                                                       Buffer_Pool_Pointer);

   procedure Parse_Action(mem       : in Memory_Pointer;
                          spacing   : in Time_Type;
                          data      : in Stream_Buffer_Pointer;
                          mdata     : in out Memory_Access;
                          state     : in out Parse_State_Type);

   protected body Buffer_Pool_Type is

      procedure Initialize is
      begin
         available := buffers'Length;
         for i in buffers'Range loop
            buffers(i) := new Stream_Buffer_Type;
         end loop;
      end Initialize;

      entry Reset when available = buffers'Length is
      begin
         null;
      end Reset;

      entry Destroy when available = buffers'Length is
      begin
         for i in buffers'Range loop
            Destroy(buffers(i));
         end loop;
      end Destroy;

      entry Allocate(sd : out Stream_Buffer_Pointer) when available > 0 is
      begin
         for i in buffers'Range loop
            if buffers(i) /= null then
               sd := buffers(i);
               buffers(i) := null;
               available := available - 1;
               return;
            end if;
         end loop;
         sd := null;
      end Allocate;

      procedure Release(sd : in Stream_Buffer_Pointer) is
      begin
         for i in buffers'Range loop
            if buffers(i) = null then
               buffers(i) := sd;
               available := available + 1;
               exit;
            end if;
         end loop;
      end Release;

   end Buffer_Pool_Type;

   task body Consumer_Type is
      pool        : Buffer_Pool_Pointer;
      mdata       : Memory_Access;
      state       : Parse_State_Type := State_Action;
      mem         : Memory_Pointer;
      spacing     : Time_Type;
      buffer      : Stream_Buffer_Pointer;
      total       : Byte_Count_Type := 0;
      skip        : Boolean := False;
   begin
      accept Initialize(m : in Memory_Pointer;
                        p : in Buffer_Pool_Pointer;
                        s : in Time_Type) do
         mem := m;
         pool := p;
         spacing := s;
      end Initialize;
      loop
         select
            accept Process(b : in Stream_Buffer_Pointer) do
               buffer := b;
            end Process;
            if not skip then
               begin
                  Parse_Action(mem, spacing, buffer, mdata, state);
               exception
                  when Prune_Error =>
                     skip := True;
               end;
            end if;
            total := total +
                     Byte_Count_Type(buffer.last - buffer.buffer'First + 1);
            Put_Line("Processed" & Byte_Count_Type'Image(total) & " bytes");
            pool.Release(buffer);
         or
            accept Reset do
               Show_Stats(mem.all);
               Reset(mem.all);
               state := State_Action;
               skip := False;
            end Reset;
         or
            terminate;
         end select;
      end loop;
   exception
      when ex: others =>
         Put_Line("error: " & Exception_Name(ex) & ": " &
                  Exception_Message(ex));
         raise;
   end Consumer_Type;

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

   procedure Process_Action(mem     : in Memory_Pointer;
                            spacing : in Time_Type;
                            mdata   : in Memory_Access) is
   begin
      case mdata.t is
         when Read   =>
            Read(mem.all, mdata.value, mdata.size);
         when Write  =>
            Write(mem.all, mdata.value, mdata.size);
         when Modify =>
            Read(mem.all, mdata.value, mdata.size);
            Write(mem.all, mdata.value, mdata.size);
         when Idle   =>
            Idle(mem.all, Time_Type(mdata.value));
      end case;
      if spacing > 0 then
         Idle(mem.all, spacing);
      end if;
   end Process_Action;

   procedure Parse_Action(mem       : in Memory_Pointer;
                          spacing   : in Time_Type;
                          data      : in Stream_Buffer_Pointer;
                          mdata     : in out Memory_Access;
                          state     : in out Parse_State_Type) is

      value : Address_Type;
      ch    : Character;

   begin
      while data.last >= data.pos loop
         ch := Character'Val(data.buffer(data.pos));
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
               data.pos := data.pos + 1;
            when State_Pre_Address =>
               mdata.value := To_Address(ch);
               if mdata.value < 16 then
                  state := State_Address;
               end if;
               data.pos := data.pos + 1;
            when State_Address =>
               value := To_Address(ch);
               if value < 16 then
                  mdata.value := mdata.value * 16 + value;
                  data.pos := data.pos + 1;
               elsif mdata.t = Idle then
                  mdata.size := 1;
                  state := State_Action;
                  Process_Action(mem, spacing, mdata);
               else
                  state := State_Pre_Size;
                  data.pos := data.pos + 1;
               end if;
            when State_Pre_Size =>
               mdata.size := Natural(To_Address(ch));
               if mdata.size < 16 then
                  state := State_Size;
               end if;
               data.pos := data.pos + 1;
            when State_Size =>
               value := To_Address(ch);
               if value < 16 then
                  mdata.size := mdata.size * 16 + Natural(value);
                  data.pos := data.pos + 1;
               else
                  state := State_Action;
                  Process_Action(mem, spacing, mdata);
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
         benchmark.file_name := To_Unbounded_String(value);
      elsif Check_Argument(arg, "iterations") then
         benchmark.iterations := Long_Integer'Value(value);
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in out Trace_Type) is
      file     : Stream_IO.File_Type;
      sdata    : Stream_Buffer_Pointer;
      pool     : Buffer_Pool_Pointer := new Buffer_Pool_Type;
      consumer : Consumer_Type;
   begin
      pool.Initialize;
      consumer.Initialize(benchmark.mem,
                                    pool,
                                    benchmark.spacing);
      Stream_IO.Open(File => file,
                     Mode => Stream_IO.In_File,
                     Name => To_String(benchmark.file_name));
      for count in 1 .. benchmark.iterations loop
         Put_Line("Iteration" & Long_Integer'Image(count) & " /" &
                  Long_Integer'Image(benchmark.iterations));
         loop
            pool.Allocate(sdata);
            Stream_IO.Read(file, sdata.buffer, sdata.last);
            exit when sdata.last < sdata.buffer'First;
            sdata.pos := sdata.buffer'First;
            consumer.Process(sdata);
         end loop;
         pool.Release(sdata);
         pool.Reset;
         if count < benchmark.iterations then
            consumer.Reset;
            Stream_IO.Reset(file);
         end if;
      end loop;
      Stream_IO.Close(file);
      pool.Destroy;
      Destroy(pool);
   exception
      when ex: others =>
         Put_Line("error: could not read " & To_String(benchmark.file_name) &
                  ": " & Exception_Name(ex) & ": " & Exception_Message(ex));
         Stream_IO.Close(file);
         pool.Destroy;
         Destroy(pool);
   end Run;

end Benchmark.Trace;

