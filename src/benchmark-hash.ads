
package Benchmark.Hash is

   type Hash_Type is new Benchmark_Type with private;

   function Create_Hash return Benchmark_Pointer;

   procedure Set_Argument(benchmark : in out Hash_Type;
                          arg       : in String);

private

   type Hash_Type is new Benchmark_Type with record
      size        : Positive := 1024;
      iterations  : Positive := 1000;
   end record;

   overriding
   procedure Run(benchmark : in out Hash_Type);

end Benchmark.Hash;
