
with Ada.Text_IO; use Ada.Text_IO;

package body Benchmark_Runner is

   procedure Register_Benchmark(runner    : in out Runner_Type;
                                benchmark : in Benchmark_Pointer) is
   begin
      runner.benchmarks.Append(benchmark);
   end Register_Benchmark;

   procedure Run(runner : in out Runner_Type;
                 mem    : in Memory_Pointer) is
      first : constant Natural := runner.benchmarks.First_Index;
      last  : constant Natural := runner.benchmarks.Last_Index;
      bp    : Benchmark_Pointer;
   begin
      for i in first .. last loop
         bp := runner.benchmarks.Element(i);
         Set_Memory(bp.all, mem);
      end loop;
      loop
         for i in reverse first .. last loop
            bp := runner.benchmarks.Element(i);
            Reset(bp.all, i);
            begin
               Run(bp.all);
            exception
               when Prune_Error =>
                  null;
            end;
            Show_Stats(mem.all);
         end loop;
         exit when Done(mem.all);
      end loop;
   exception
      when Invalid_Address =>
         Put_Line("error: invalid address");
   end Run;

   procedure Finalize(runner : in out Runner_Type) is
      first : constant Integer := runner.benchmarks.First_Index;
      last  : constant Integer := runner.benchmarks.Last_Index;
      bp    : Benchmark_Pointer;
   begin
      for i in first .. last loop
         bp := runner.benchmarks.Element(i);
         Destroy(bp);
      end loop;
   end Finalize;

end Benchmark_Runner;
