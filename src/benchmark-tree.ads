
package Benchmark.Tree is

   type Tree_Type is new Benchmark_Type with private;

   function Create_Tree return Benchmark_Pointer;

   overriding
   procedure Set_Argument(benchmark    : in out Tree_Type;
                          arg          : in String);

   overriding
   procedure Run(benchmark : in Tree_Type);

private

   type Tree_Type is new Benchmark_Type with record
      size        : Positive  := 1024;
      iterations  : Positive  := 10000;
   end record;

end Benchmark.Tree;
