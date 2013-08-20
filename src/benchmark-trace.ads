
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

   overriding
   procedure Set_Argument(benchmark : in out Trace_Type;
                          arg       : in String);

private

   Buffer_Size    : constant := 2 ** 24;

   package File_Vectors is new Vectors(Natural, Unbounded_String);

   type Trace_Type is new Benchmark_Type with record
      file_names  : File_Vectors.Vector;
      buffer      : Stream_Element_Array(1 .. Buffer_Size);
      pos         : Stream_Element_Offset := Stream_Element_Offset'Last;
      last        : Stream_Element_Offset := Stream_Element_Offset'First;
      context     : Natural := 0;
   end record;

   overriding
   procedure Reset(benchmark : in out Trace_Type);

   overriding
   procedure Run(benchmark : in out Trace_Type);


end Benchmark.Trace;
