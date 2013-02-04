
with Ada.Command_Line;  use Ada.Command_Line;
with Ada.Text_IO;       use Ada.Text_IO;
with Memory;            use Memory;
with Parser;
with Benchmark;         use Benchmark;
with Benchmark.Heap;
with Benchmark.Trace;

procedure Main is

   mem   : Memory_Pointer := null;
   bm    : Benchmark.Benchmark_Pointer := null;

begin

   -- Make sure we have enough arguments.
   if Argument_Count < 2 then
      Put_Line("usage: " & Command_Name & " <memory> <benchmark> [<options>]");
      Put_Line("benchmarks:");
      Put_Line("   trace [file=trace.txt] [spacing=0]");
      Put_Line("   heap [size=1024] [iterations=1000]");
      return;
   end if;

   -- Parse the memory file.
   mem := Parser.Parse(Argument(1));
   if mem = null then
      Put_Line("error: could not open memory: " & Argument(1));
      return;
   end if;

   -- Create the benchmark.
   if Argument(2) = "heap" then
      bm := new Benchmark.Heap.Heap_Type;
   elsif Argument(2) = "trace" then
      bm := new Benchmark.Trace.Trace_Type;
   end if;
   if bm = null then
      Put_Line("error: invalid benchmark: " & Argument(2));
      return;
   end if;

   -- Set benchmark arguments.
   for i in 3 .. Argument_Count loop
      begin
         Benchmark.Set_Argument(bm.all, Argument(i));
      exception
         when Benchmark.Invalid_Argument =>
            Put_Line("error: invalid argument " & Argument(i));
            return;
      end;
   end loop;

   Benchmark.Run(bm.all, mem);
   Show_Stats(mem.all);
   Destroy(bm);
   Destroy(mem);

end Main;

