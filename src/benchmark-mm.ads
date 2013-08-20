
package Benchmark.MM is

   type MM_Type is new Benchmark_Type with private;

   function Create_MM return Benchmark_Pointer;

   procedure Set_Argument(benchmark : in out MM_Type;
                          arg       : in String);

private

   type MM_Type is new Benchmark_Type with record
      size        : Positive := 256;
      iterations  : Positive := 1;
   end record;

   overriding
   procedure Run(benchmark : in out MM_Type);

end Benchmark.MM;
