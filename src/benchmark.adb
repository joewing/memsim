
package body Benchmark is

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

   procedure Write(benchmark  : Benchmark_Type'class;
                   address    : Natural;
                   value      : Integer) is
   begin
      Memory.Write(benchmark.mem.all, Memory.Address_Type(address));
      if address > benchmark.data.Length then
         benchmark.data.Set_Length(address + 1);
      end if;
      benchmark.data.Replace_Element(address, value);
   end Write;

end Benchmark;
