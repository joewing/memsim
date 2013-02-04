
package Benchmark.Heap is

   type Heap_Type is new Benchmark_Type with private;

private

   type Heap_Type is new Benchmark_Type with null record;

   procedure Run(benchmark : in out Heap_Type);

end Benchmark.Heap;
