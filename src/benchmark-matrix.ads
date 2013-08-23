
package Benchmark.Matrix is

   type Matrix_Type is abstract new Benchmark_Type with private;

   overriding
   procedure Set_Argument(benchmark : in out Matrix_Type;
                          arg       : in String);

   overriding
   procedure Run(benchmark : in Matrix_Type) is abstract;

private

   type Matrix_Type is abstract new Benchmark_Type with record
      size        : Positive     := 256;
      iterations  : Positive     := 1;
   end record;

   function Get_Size(benchmark : Matrix_Type'Class) return Address_Type;

   procedure Read(benchmark   : in Matrix_Type'Class;
                  offset      : in Address_Type;
                  x, y        : in Natural);

   procedure Write(benchmark  : in Matrix_Type'Class;
                   offset     : in Address_Type;
                   x, y       : in Natural);

end Benchmark.Matrix;
