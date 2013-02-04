
package body Benchmark is

   procedure Set_Run_Length(benchmark  : in out Benchmark_Type'class;
                            length     : in Positive) is
   begin
      benchmark.length := length;
   end Set_Run_Length;

   procedure Set_Size(benchmark  : in out Benchmark_Type'class;
                      size       : in Positive) is
   begin
      benchmark.size := size;
   end Set_Size;

   procedure Run(benchmark : in out Benchmark_Type'class;
                 mem       : in Memory.Memory_Pointer) is
   begin
      benchmark.mem := mem;
      Run(benchmark);
   end Run;

   function Get_Random(benchmark : Benchmark_Type'class) return Natural is
   begin
      return Random.Random(benchmark.generator);
   end Get_Random;

   function Read(benchmark : Benchmark_Type'class;
                 address   : Natural) return Integer is
   begin
      Memory.Read(benchmark.mem.all, Memory.Address_Type(address));
      return benchmark.data.Element(address);
   end Read;

   procedure Write(benchmark  : in out Benchmark_Type'class;
                   address    : in Natural;
                   value      : in Integer) is
   begin
      Memory.Write(benchmark.mem.all, Memory.Address_Type(address));
      if Count_Type(address) >= benchmark.data.Length then
         benchmark.data.Set_Length(Count_Type(address + 1));
      end if;
      benchmark.data.Replace_Element(address, value);
   end Write;

end Benchmark;
