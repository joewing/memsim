
with Ada.Unchecked_Deallocation;
with Device; use Device;

package body Benchmark is

   procedure Set_Memory(benchmark   : in out Benchmark_Type'Class;
                        mem         : in Memory_Pointer) is
   begin
      benchmark.max_addr
         := (Address_Type(2) ** Get_Address_Bits) - 1;
      benchmark.mem := mem;
   end Set_Memory;

   procedure Reset(benchmark  : in out Benchmark_Type'Class;
                   context    : in Natural) is
   begin
      Random.Reset(benchmark.generator, benchmark.seed);
      Reset(benchmark.mem.all, context);
      benchmark.data.all.Clear;
   end Reset;

   procedure Set_Argument(benchmark : in out Benchmark_Type;
                          arg       : in String) is
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "spacing") then
         benchmark.spacing := Time_Type'Value(value);
      elsif Check_Argument(arg, "seed") then
         benchmark.seed := Integer'Value(value);
      else
         raise Invalid_Argument;
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   function Check_Argument(arg   : String;
                           name  : String) return Boolean is
      full_name   : constant String := name & "=";
      len         : constant Natural := full_name'Length;
   begin
      if len < arg'Length then
         return arg(arg'First .. arg'First + len - 1) = full_name;
      else
         return False;
      end if;
   end Check_Argument;

   function Extract_Argument(arg : String) return String is
   begin
      for i in arg'First .. arg'Last loop
         if arg(i) = '=' then
            return arg(i + 1 .. arg'Last);
         end if;
      end loop;
      return "";
   end Extract_Argument;

   function Get_Random(benchmark : Benchmark_Type'Class) return Natural is
   begin
      return Random.Random(benchmark.generator);
   end Get_Random;

   function Read_Value(benchmark : Benchmark_Type'Class;
                       address   : Natural) return Integer is
   begin
      Read(benchmark, Address_Type(address * 4), 4);
      Idle(benchmark, benchmark.spacing);
      return benchmark.data.Element(address);
   end Read_Value;

   procedure Write_Value(benchmark  : in Benchmark_Type'Class;
                         address    : in Natural;
                         value      : in Integer) is
   begin
      Write(benchmark, Address_Type(address * 4), 4);
      Idle(benchmark, benchmark.spacing);
      if Count_Type(address) >= benchmark.data.Length then
         benchmark.data.Set_Length(Count_Type(address + 1));
      end if;
      benchmark.data.Replace_Element(address, value);
   end Write_Value;

   procedure Read(benchmark   : in Benchmark_Type'Class;
                  address     : in Address_Type;
                  size        : in Positive) is
   begin
      if address + Address_Type(size) > benchmark.max_addr then
         raise Invalid_Address;
      end if;
      Read(benchmark.mem.all, address, size);
   end Read;

   procedure Write(benchmark  : in Benchmark_Type'Class;
                   address    : in Address_Type;
                   size       : in Positive) is
   begin
      if address + Address_Type(size) > benchmark.max_addr then
         raise Invalid_Address;
      end if;
      Write(benchmark.mem.all, address, size);
   end Write;

   procedure Idle(benchmark   : in Benchmark_Type'Class;
                  cycles      : in Time_Type) is
   begin
      if cycles > 0 then
         Idle(benchmark.mem.all, cycles);
      end if;
   end Idle;

   procedure Free is
      new Ada.Unchecked_Deallocation(Benchmark_Type'Class, Benchmark_Pointer);

   procedure Free is
      new Ada.Unchecked_Deallocation(Data_Vectors.Vector, Data_Pointer);

   procedure Initialize(benchmark : in out Benchmark_Type) is
   begin
      benchmark.data := new Data_Vectors.Vector;
   end Initialize;

   procedure Finalize(benchmark : in out Benchmark_Type) is
   begin
      Free(benchmark.data);
   end Finalize;

   procedure Destroy(benchmark : in out Benchmark_Pointer) is
   begin
      Free(benchmark);
   end Destroy;

end Benchmark;
