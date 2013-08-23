
package Benchmark.Matrix.LU is

   type LU_Type is new Matrix_Type with private;

   function Create_LU return Benchmark_Pointer;

   overriding
   procedure Run(benchmark : in LU_Type);

private

   type LU_Type is new Matrix_Type with null record;

end Benchmark.Matrix.LU;
