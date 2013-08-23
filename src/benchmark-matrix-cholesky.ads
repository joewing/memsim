
package Benchmark.Matrix.Cholesky is

   type Cholesky_Type is new Matrix_Type with private;

   function Create_Cholesky return Benchmark_Pointer;

   overriding
   procedure Run(benchmark : in Cholesky_Type);

private

   type Cholesky_Type is new Matrix_Type with null record;

end Benchmark.Matrix.Cholesky;
