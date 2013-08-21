
with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Finalization;        use Ada.Finalization;
with Benchmark;               use Benchmark;
with Memory;                  use Memory;

package Benchmark_Runner is

   type Runner_Type is new Limited_Controlled with private;

   procedure Register_Benchmark(runner    : in out Runner_Type;
                                benchmark : in Benchmark_Pointer);

   procedure Run(runner : in out Runner_Type;
                 mem    : in Memory_Pointer);

private

   package Benchmark_Vectors is new Vectors(Natural, Benchmark_Pointer);

   type Runner_Type is new Limited_Controlled with record
      benchmarks : Benchmark_Vectors.Vector;
   end record;

   overriding
   procedure Finalize(runner : in out Runner_Type);

end Benchmark_Runner;
