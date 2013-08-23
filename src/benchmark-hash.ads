
package Benchmark.Hash is

   type Hash_Type is new Benchmark_Type with private;

   function Create_Hash return Benchmark_Pointer;

   overriding
   procedure Set_Argument(benchmark : in out Hash_Type;
                          arg       : in String);

   overriding
   procedure Run(benchmark : in Hash_Type);

private

   type Hash_Type is new Benchmark_Type with record
      size        : Positive := 1024;
      iterations  : Positive := 1000;
   end record;

end Benchmark.Hash;
