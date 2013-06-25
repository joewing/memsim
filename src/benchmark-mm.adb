
package body Benchmark.MM is

   function Create_MM return Benchmark_Pointer is
   begin
      return new MM_Type;
   end Create_MM;

   procedure Set_Argument(benchmark : in out MM_Type;
                          arg       : in String) is
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "size") then
         benchmark.size := Positive'Value(value);
      elsif Check_Argument(arg, "iterations") then
         benchmark.iterations := Positive'Value(value);
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in out MM_Type) is
      size  : constant Address_Type := Address_Type(benchmark.size);
      msize : constant Address_Type := size * size * 4;
      srca  : constant Address_Type := 0 * msize;
      srcb  : constant Address_Type := 1 * msize;
      dest  : constant Address_Type := 2 * msize;
   begin
      for i in 1 .. benchmark.iterations loop
         for a in 0 .. Address_Type(benchmark.size - 1) loop
            for b in 0 .. Address_Type(benchmark.size - 1) loop
               Write(benchmark, dest + (a * size + b) * 4, 4);
               Idle(benchmark, benchmark.spacing);
               for c in 0 .. Address_Type(benchmark.size - 1) loop
                  Read(benchmark, srca + (b * size + c) * 4, 4);
                  Idle(benchmark, benchmark.spacing);
                  Read(benchmark, srcb + (c * size + a) * 4, 4);
                  Idle(benchmark, benchmark.spacing);
                  Write(benchmark, dest + (a * size + b) * 4, 4);
                  Idle(benchmark, benchmark.spacing);
               end loop;
            end loop;
         end loop;
      end loop;
   end Run;

end Benchmark.MM;
