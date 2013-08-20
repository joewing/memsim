
package Benchmark.Heap is

   type Heap_Type is new Benchmark_Type with private;

   function Create_Heap return Benchmark_Pointer;

   procedure Set_Argument(benchmark : in out Heap_Type;
                          arg       : in String);

private

   type Heap_Type is new Benchmark_Type with record
      size        : Positive := 1024;
   end record;

   overriding
   procedure Run(benchmark : in out Heap_Type);

end Benchmark.Heap;
