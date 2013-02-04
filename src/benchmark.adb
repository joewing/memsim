
with Ada.Unchecked_Deallocation;

package body Benchmark is

   procedure Run(benchmark : in out Benchmark_Type'Class;
                 mem       : in Memory.Memory_Pointer) is
   begin
      benchmark.mem := mem;
      Run(benchmark);
   end Run;

   procedure Set_Argument(benchmark : in out Benchmark_Type;
                          arg       : in String) is
   begin
      raise Invalid_Argument;
   end Set_Argument;

   function Get_Random(benchmark : Benchmark_Type'Class) return Natural is
   begin
      return Random.Random(benchmark.generator);
   end Get_Random;

   function Read(benchmark : Benchmark_Type'Class;
                 address   : Natural) return Integer is
   begin
      Memory.Read(benchmark.mem.all, Memory.Address_Type(address));
      return benchmark.data.Element(address);
   end Read;

   procedure Write(benchmark  : in out Benchmark_Type'Class;
                   address    : in Natural;
                   value      : in Integer) is
   begin
      Memory.Write(benchmark.mem.all, Memory.Address_Type(address));
      if Count_Type(address) >= benchmark.data.Length then
         benchmark.data.Set_Length(Count_Type(address + 1));
      end if;
      benchmark.data.Replace_Element(address, value);
   end Write;

   procedure Idle(benchmark   : in out Benchmark_Type'Class;
                  cycles      : in Time_Type) is
   begin
      Memory.Idle(benchmark.mem.all, cycles);
   end Idle;

   procedure Deallocate is
      new Ada.Unchecked_Deallocation(Benchmark_Type'Class, Benchmark_Pointer);

   procedure Destroy(benchmark : in out Benchmark_Pointer) is
   begin
      Deallocate(benchmark);
   end Destroy;

end Benchmark;
