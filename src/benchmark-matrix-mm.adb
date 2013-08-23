
package body Benchmark.Matrix.MM is

   function Create_MM return Benchmark_Pointer is
   begin
      return new MM_Type;
   end Create_MM;

   procedure Run(benchmark : in MM_Type) is
      msize : constant Address_Type := Get_Size(benchmark);
      srca  : constant Address_Type := 0 * msize;
      srcb  : constant Address_Type := 1 * msize;
      dest  : constant Address_Type := 2 * msize;
   begin
      for i in 1 .. benchmark.iterations loop
         for a in 0 .. benchmark.size - 1 loop
            for b in 0 .. benchmark.size - 1 loop
               Write(benchmark, dest, a, b);
               Idle(benchmark, benchmark.spacing);
               for c in 0 .. benchmark.size - 1 loop
                  Read(benchmark, srca, b, c);
                  Idle(benchmark, benchmark.spacing);
                  Read(benchmark, srcb, c, a);
                  Idle(benchmark, benchmark.spacing);
                  Write(benchmark, dest, a, b);
                  Idle(benchmark, benchmark.spacing);
               end loop;
            end loop;
         end loop;
      end loop;
   end Run;

end Benchmark.Matrix.MM;
