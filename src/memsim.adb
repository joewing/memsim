
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with Memory;                  use Memory;
with Parser;
with Benchmark;               use Benchmark;
with Benchmark.Heap;
with Benchmark.Trace;
with Benchmark.Stride;
with Benchmark.Hash;
with Benchmark.MM;
with Test;

procedure MemSim is

   mem   : Memory_Pointer := null;
   bm    : Benchmark.Benchmark_Pointer := null;

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
      BM_Entry("hash",
               "[size=1024][iterations=1000][spacing=0]",
               Benchmark.Hash.Create_Hash'Access),
      BM_Entry("heap",
               "[size=1024][iterations=1000][spacing=0]",
               Benchmark.Heap.Create_Heap'Access),
      BM_Entry("mm",
               "[size=256][iterations=1][spacing=0]",
               Benchmark.MM.Create_MM'Access),
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
   elsif Argument_Count < 2 then
      Put_Line("usage: " & Command_Name & " <memory> <benchmark> [<options>]");
      Put_Line("benchmarks:");
      for i in benchmark_map'First .. benchmark_map'Last loop
         Put_Line("   " & To_String(benchmark_map(i).name & " " &
                  To_String(benchmark_map(i).usage)));
      end loop;
      return;
   end if;

   declare
      memory_file : constant String := Argument(1);
      bm_name     : constant String := Argument(2);
   begin

      -- Parse the memory file.
      mem := Parser.Parse(memory_file);
      if mem = null then
         Put_Line("error: could not open memory: " & memory_file);
         return;
      end if;

      -- Create the benchmark.
      for i in benchmark_map'First .. benchmark_map'Last loop
         if benchmark_map(i).name = bm_name then
            bm := benchmark_map(i).constructor.all;
            exit;
         end if;
      end loop;
      if bm = null then
         Put_Line("error: invalid benchmark: " & Argument(2));
         return;
      end if;

   end;

   -- Set benchmark arguments.
   for i in 3 .. Argument_Count loop
      begin
         Benchmark.Set_Argument(bm.all, Argument(i));
      exception
         when Benchmark.Invalid_Argument =>
            Put_Line("error: invalid argument " & Argument(i));
            return;
      end;
   end loop;

   Benchmark.Run(bm.all, mem);
   Show_Stats(mem.all);
   Destroy(bm);
   Destroy(mem);

end MemSim;

