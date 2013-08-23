
package Benchmark.Cholesky is

   type Cholesky_Type is new Benchmark_Type with private;

   function Create_Cholesky return Benchmark_Pointer;

   procedure Set_Argument(benchmark : in out Cholesky_Type;
                          arg       : in String);

private

   type Cholesky_Type is new Benchmark_Type with record
      size : Positive := 64;
   end record;

   overriding
   procedure Run(benchmark : in out Cholesky_Type);

end Benchmark.Cholesky;
