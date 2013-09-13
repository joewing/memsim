

package Benchmark.QSort is

   type QSort_Type is new Benchmark_Type with private;

   function Create_QSort return Benchmark_Pointer;

   overriding
   procedure Set_Argument(benchmark : in out QSort_Type;
                          arg       : in String);

   overriding
   procedure Run(benchmark : in QSort_Type);

private

   type QSort_Type is new Benchmark_Type with record
      size     : Positive := 1024;
   end record;

end Benchmark.QSort;
