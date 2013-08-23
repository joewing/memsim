
package Benchmark.Heap is

   type Heap_Type is new Benchmark_Type with private;

   function Create_Heap return Benchmark_Pointer;

   overriding
   procedure Set_Argument(benchmark : in out Heap_Type;
                          arg       : in String);

   overriding
   procedure Run(benchmark : in Heap_Type);

private

   type Heap_Type is new Benchmark_Type with record
      size        : Positive := 1024;
   end record;

end Benchmark.Heap;
