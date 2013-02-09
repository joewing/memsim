
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Streams;           use Ada.Streams;

package Benchmark.Trace is

   -- Memory traces are sequences of memory access actions separated by
   -- new lines.  The format of each line is:
   --
   --    <action> <value>
   --
   -- where <action> is 'R' for read, 'W' for write, and 'I' for idle.
   -- For reads and writes, <value> is a hexadecimal value indicating the
   -- address.  For idle states, <value> is the number of idle cycles.
   -- Idle is used to separate reads and writes.

   type Trace_Type is new Benchmark_Type with private;

   type Trace_Pointer is access all Trace_Type'Class;

   function Create_Trace return Benchmark_Pointer;

   procedure Set_Argument(benchmark : in out Trace_Type;
                          arg       : in String);

   procedure Run(benchmark : in out Trace_Type);

private

   Buffer_Size    : constant := 2 ** 24;
   Buffer_Count   : constant := 2;

   type Stream_Buffer_Type is record
      buffer   : Stream_Element_Array(1 .. Buffer_Size);
      pos      : Stream_Element_Offset := Stream_Element_Offset'Last;
      last     : Stream_Element_Offset := Stream_Element_Offset'First;
   end record;

   type Stream_Buffer_Pointer is access all Stream_Buffer_Type;

   type Stream_Buffer_Array is
      array(1 .. Buffer_Count) of Stream_Buffer_Pointer;

   protected type Buffer_Pool_Type is
      procedure Initialize;
      entry Allocate(sd : out Stream_Buffer_Pointer);
      procedure Release(sd  : in Stream_Buffer_Pointer);
      entry Destroy;
   private
      available   : Natural;
      buffers     : Stream_Buffer_Array;
   end Buffer_Pool_Type;

   type Buffer_Pool_Pointer is access all Buffer_Pool_Type;

   task type Consumer_Type is
      entry Initialize(m : in Memory_Pointer;
                       p : in Buffer_Pool_Pointer;
                       s : in Time_Type);
      entry Process(b : in Stream_Buffer_Pointer);
   end Consumer_Type;

   type Trace_Type is new Benchmark_Type with record
      file_name   : Unbounded_String := To_Unbounded_String("trace.txt");
   end record;

end Benchmark.Trace;
