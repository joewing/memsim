
package body Benchmark.Matrix.Cholesky is

   function Create_Cholesky return Benchmark_Pointer is
   begin
      return new Cholesky_Type;
   end Create_Cholesky;

   procedure Run(benchmark : in Cholesky_Type) is
      addr : Natural;
   begin
      for i in 0 .. benchmark.size - 1 loop
         addr := i * benchmark.size + i;
         Read(benchmark, Address_Type(addr) * 4, 4);
         for j in 0 .. i loop
            addr := i * benchmark.size + j;
            Read(benchmark, Address_Type(addr) * 4, 4);
         end loop;
         for j in i + 1 .. benchmark.size - 1 loop
            addr := i * benchmark.size + j;
            Read(benchmark, Address_Type(addr) * 4, 4);
            for k in 0 .. i loop
               addr := j * benchmark.size + k;
               Read(benchmark, Address_Type(addr) * 4, 4);
               addr := i * benchmark.size + k;
               Read(benchmark, Address_Type(addr) * 4, 4);
            end loop;
            addr := j * benchmark.size + i;
            Write(benchmark, Address_Type(addr) * 4, 4);
         end loop;
      end loop;
   end Run;

end Benchmark.Matrix.Cholesky;
