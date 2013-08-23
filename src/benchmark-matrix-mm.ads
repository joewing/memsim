
package Benchmark.Matrix.MM is

   type MM_Type is new Matrix_Type with private;

   function Create_MM return Benchmark_Pointer;

   overriding
   procedure Run(benchmark : in MM_Type);

private

   type MM_Type is new Matrix_Type with null record;

end Benchmark.Matrix.MM;
