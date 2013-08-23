
package Benchmark.Stride is

   type Stride_Type is new Benchmark_Type with private;

   function Create_Stride return Benchmark_Pointer;

   overriding
   procedure Set_Argument(benchmark : in out Stride_Type;
                          arg       : in String);

   overriding
   procedure Run(benchmark : in Stride_Type);

private

   type Stride_Type is new Benchmark_Type with record
      size        : Positive := 1024;
      stride      : Integer := 1;
      iterations  : Positive := 1000;
   end record;

end Benchmark.Stride;
