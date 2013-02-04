
with Memory;   use Memory;
with Trace;    use Trace;
with Parser;

with Benchmark;
with Benchmark.Heap;

procedure Main is

   mem      : Memory_Pointer := null;

   heap     : Benchmark.Heap.Heap_Type;

begin

   mem := Parser.Parse("memory.txt");
   if mem /= null then
--      Process(mem.all, "trace.txt", 50);
      Benchmark.Run(heap, mem);
      Show_Stats(mem.all);
      Destroy(mem);
   end if;

end Main;

