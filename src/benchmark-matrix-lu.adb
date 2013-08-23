
package body Benchmark.Matrix.LU is

   function Create_LU return Benchmark_Pointer is
   begin
      return new LU_Type;
   end Create_LU;

   procedure Run(benchmark : in LU_Type) is
      addr : constant Address_Type := 0;
   begin
      for k in 0 .. benchmark.size - 1 loop
         for j in k + 1 .. benchmark.size - 1 loop
            Read(benchmark, addr, k, j);
            Read(benchmark, addr, k, k);
            Write(benchmark, addr, k, j);
         end loop;
         for i in k + 1 .. benchmark.size - 1 loop
            for j in k + 1 .. benchmark.size - 1 loop
               Read(benchmark, addr, i, j);
               Read(benchmark, addr, i, k);
               Read(benchmark, addr, k, j);
               Write(benchmark, addr, i, j);
            end loop;
         end loop;
      end loop;
   end Run;

end Benchmark.Matrix.LU;
