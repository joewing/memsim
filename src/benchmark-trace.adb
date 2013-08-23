
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Streams;                use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

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

   Buffer_Size    : constant := 2 ** 16;

   type Trace_Context_Type is record
      buffer      : Stream_Element_Array(1 .. Buffer_Size);
      pos         : Stream_Element_Offset := Stream_Element_Offset'Last;
      last        : Stream_Element_Offset := Stream_Element_Offset'First;
      state       : Parse_State_Type := State_Action;
   end record;

   type Trace_Context_Pointer is access Trace_Context_Type;

   procedure Parse_Action(benchmark : in Trace_Type;
                          context   : in out Trace_Context_Type;
                          mdata     : in out Memory_Access);

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

   procedure Parse_Action(benchmark : in Trace_Type;
                          context   : in out Trace_Context_Type;
                          mdata     : in out Memory_Access) is

      value : Address_Type;
      ch    : Character;

   begin
      while context.last >= context.pos loop
         ch := Character'Val(context.buffer(context.pos));
         case context.state is
            when State_Action =>
               context.state := State_Pre_Address;
               case ch is
                  when 'R'    => mdata.t := Read;
                  when 'W'    => mdata.t := Write;
                  when 'M'    => mdata.t := Modify;
                  when 'I'    => mdata.t := Idle;
                  when others => context.state := State_Action;
               end case;
               context.pos := context.pos + 1;
            when State_Pre_Address =>
               mdata.value := To_Address(ch);
               if mdata.value < 16 then
                  context.state := State_Address;
                  context.pos := context.pos + 1;
               else
                  context.state := State_Action;
               end if;
            when State_Address =>
               value := To_Address(ch);
               if value < 16 then
                  mdata.value := mdata.value * 16 + value;
                  context.pos := context.pos + 1;
               elsif mdata.t = Idle then
                  mdata.size := 1;
                  context.state := State_Action;
                  Process_Action(benchmark, mdata);
               else
                  context.state := State_Pre_Size;
                  context.pos := context.pos + 1;
               end if;
            when State_Pre_Size =>
               mdata.size := Natural(To_Address(ch));
               if mdata.size < 16 then
                  context.state := State_Size;
               end if;
               context.pos := context.pos + 1;
            when State_Size =>
               value := To_Address(ch);
               if value < 16 then
                  mdata.size := mdata.size * 16 + Natural(value);
                  context.pos := context.pos + 1;
               else
                  context.state := State_Action;
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
         benchmark.file_name := To_Unbounded_String(value);
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Destroy is new Ada.Unchecked_Deallocation(Trace_Context_Type,
                                                       Trace_Context_Pointer);

   procedure Run(benchmark : in Trace_Type) is
      file     : Stream_IO.File_Type;
      context  : Trace_Context_Pointer := new Trace_Context_Type;
      mdata    : Memory_Access;
   begin
      Stream_IO.Open(File => file,
                     Mode => Stream_IO.In_File,
                     Name => To_String(benchmark.file_name));
      loop
         Stream_IO.Read(file, context.buffer, context.last);
         exit when context.last < context.buffer'First;
         context.pos := context.buffer'First;
         Parse_Action(benchmark, context.all, mdata);
      end loop;
      context.buffer(context.buffer'First) := 0;
      context.pos := context.buffer'First;
      context.last := context.pos;
      Parse_Action(benchmark, context.all, mdata);
      Stream_IO.Close(file);
      Destroy(context);
   exception
      when ex: Device_Error =>
         Put_Line("error: could not read " & To_String(benchmark.file_name) &
                  ": " & Exception_Name(ex) & ": " & Exception_Message(ex));
         Stream_IO.Close(file);
         Destroy(context);
         raise;
      when others =>
         Stream_IO.Close(file);
         Destroy(context);
         raise;
   end Run;

end Benchmark.Trace;
