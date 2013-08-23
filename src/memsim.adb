
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Memory;                  use Memory;
with Parser;
with Benchmark;               use Benchmark;
with Benchmark_Runner;        use Benchmark_Runner;
with Benchmark.Heap;
with Benchmark.Trace;
with Benchmark.Stride;
with Benchmark.Hash;
with Benchmark.Matrix.MM;
with Benchmark.Matrix.Cholesky;
with Benchmark.Matrix.LU;
with Test;
with Util;                    use Util;

procedure MemSim is

   mem      : Memory_Pointer := null;
   runner   : Runner_Type;

   type Benchmark_Constructor_Type is
      access function return Benchmark.Benchmark_Pointer;

   type Benchmark_Info_Type is record
      name           : Unbounded_String;
      usage          : Unbounded_String;
      constructor    : Benchmark_Constructor_Type;
   end record;

   type Benchmark_Info_Array is array(Natural range <>) of Benchmark_Info_Type;

   function BM_Entry(name        : String;
                     usage       : String;
                     constructor : Benchmark_Constructor_Type)
                     return Benchmark_Info_Type is
   begin
      return Benchmark_Info_Type'(To_Unbounded_String(name),
                                  To_Unbounded_String(usage),
                                  constructor);
   end BM_Entry;

   benchmark_map : constant Benchmark_Info_Array := (
      BM_Entry("cholesky",
               "[size=256][iterations=1][spacing=0]",
               Benchmark.Matrix.Cholesky.Create_Cholesky'Access),
      BM_Entry("hash",
               "[size=1024][iterations=1000][spacing=0][seed=15]",
               Benchmark.Hash.Create_Hash'Access),
      BM_Entry("heap",
               "[size=1024][spacing=0][seed=15]",
               Benchmark.Heap.Create_Heap'Access),
      BM_Entry("lu",
               "[size=256][iterations=1][spacing=0]",
               Benchmark.Matrix.LU.Create_LU'Access),
      BM_Entry("mm",
               "[size=256][iterations=1][spacing=0][seed=15]",
               Benchmark.Matrix.MM.Create_MM'Access),
      BM_Entry("stride",
               "[size=1024][iterations=1000][stride=1][spacing=0]",
               Benchmark.Stride.Create_Stride'Access),
      BM_Entry("trace",
               "[file=trace.txt][spacing=0]",
               Benchmark.Trace.Create_Trace'Access)
   );

begin

   -- Make sure we have enough arguments.
   if Argument_Count = 1 and then Argument(1) = "test" then
      Test.Run_Tests;
      return;
   elsif Argument_Count = 2 and then Argument(2) = "show" then
      mem := Parser.Parse(Argument(1));
      if mem = null then
         Put_Line("error: could not open memory: " & Argument(1));
         return;
      end if;
      Put_Line("Max Path:" & Natural'Image(Get_Max_Length(mem)));
      Put_Line("Cost:" & Cost_Type'Image(Get_Cost(mem.all)));
      Destroy(mem);
      return;
   elsif Argument_Count < 2 then
      Put_Line("usage: " & Command_Name & " <memory> <benchmark> [<options>]");
      Put_Line("benchmarks:");
      for i in benchmark_map'First .. benchmark_map'Last loop
         Put_Line("   " & To_String(benchmark_map(i).name & " " &
                  To_String(benchmark_map(i).usage)));
      end loop;
      Put_Line("   show");
      return;
   end if;

   -- Parse the memory file.
   Parse_Memory: declare
      memory_file : constant String := Argument(1);
   begin
      mem := Parser.Parse(memory_file);
      if mem = null then
         Put_Line("error: could not open memory: " & memory_file);
         return;
      end if;
   end Parse_Memory;

   -- Parse benchmarks.
   Parse_Benchmarks: declare
      bp : Benchmark_Pointer := null;
   begin
      for i in 2 .. Argument_Count loop

         -- If this is a benchmark name, create a new benchmark.
         for b in benchmark_map'First .. benchmark_map'Last loop
            if benchmark_map(b).name = Argument(i) then
               bp := benchmark_map(b).constructor.all;
               Register_Benchmark(runner, bp);
               goto Parsed_Argument;
            end if;
         end loop;

         -- If we get here, this argument is not a benchmark.
         -- First we need to make sure we actually have a benchmark.
         if bp = null then
            Put_Line("error: invalid benchmark: " & Argument(i));
            return;
         end if;

         -- If we get here, this is an argument to the current benchmark.
         begin
            Benchmark.Set_Argument(bp.all, Argument(i));
         exception
            when Benchmark.Invalid_Argument =>
               Put_Line("error: invalid argument: " & Argument(i));
               return;
         end;

      <<Parsed_Argument>>

         null;

      end loop;
   end Parse_Benchmarks;

   Run(runner, mem);
   Destroy(mem);

end MemSim;

