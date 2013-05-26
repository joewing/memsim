
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
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "spacing") then
         benchmark.spacing := Time_Type'Value(value);
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

   procedure Write_Value(benchmark  : in out Benchmark_Type'Class;
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
      wsize    : constant Natural := Get_Word_Size(benchmark.mem.all);
      offset   : constant Natural := Natural(address mod Address_Type(wsize));
      start    : constant Address_Type := address / Address_Type(wsize);
      count    : constant Positive := (size + wsize + offset - 1) / wsize;
   begin
      for i in Address_Type range Address_Type(1) .. Address_Type(count) loop
         Read(benchmark.mem.all, (start + i - 1) * Address_Type(wsize), wsize);
      end loop;
   end Read;

   procedure Write(benchmark  : in Benchmark_Type'Class;
                   address    : in Address_Type;
                   size       : in Positive) is
      wsize    : constant Natural := Get_Word_Size(benchmark.mem.all);
      offset   : constant Natural := Natural(address mod Address_Type(wsize));
      start    : constant Address_Type := address / Address_Type(wsize);
      count    : constant Positive := (size + wsize + offset - 1) / wsize;
   begin

      -- If the write doesn't start at a word boundary, we need to
      -- read the first word.
      if offset /= 0 then
         Read(benchmark.mem.all, start * Address_Type(wsize), wsize);
      elsif offset + size < wsize then
         Read(benchmark.mem.all, start * Address_Type(wsize), wsize);
      end if;

      -- If the write doesn't end at a word boundary, we must read
      -- the last word.
      if offset + size > wsize and ((offset + size) mod wsize) /= 0 then
         Read(benchmark.mem.all,
              (start + Address_Type(count - 1)) * Address_Type(wsize), wsize);
      end if;

      -- Perform the write(s).
      for i in Address_Type range Address_Type(1) .. Address_Type(count) loop
         Write(benchmark.mem.all,
               (start + i - 1) * Address_Type(wsize), wsize);
      end loop;

   end Write;

   procedure Idle(benchmark   : in Benchmark_Type'Class;
                  cycles      : in Time_Type) is
   begin
      Idle(benchmark.mem.all, cycles);
   end Idle;

   procedure Deallocate is
      new Ada.Unchecked_Deallocation(Benchmark_Type'Class, Benchmark_Pointer);

   procedure Destroy(benchmark : in out Benchmark_Pointer) is
   begin
      Deallocate(benchmark);
   end Destroy;

end Benchmark;
