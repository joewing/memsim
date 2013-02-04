
package Benchmark.Heap is

   type Heap_Type is new Benchmark_Type with private;

   procedure Run(benchmark : in out Heap_Type);

private

   type Heap_Type is new Benchmark_Type with null record;

end Benchmark.Heap;
