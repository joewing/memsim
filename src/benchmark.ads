
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Ada.Finalization;
use Ada.Containers;
use Ada.Finalization;

with Memory;   use Memory;
with Util;     use Util;

-- Base package for benchmarks.
package Benchmark is

   -- An exception that is raised when an invalid argument is
   -- provided to a benchmark.
   Invalid_Argument : exception;

   -- The base data type for benchmarks.
   type Benchmark_Type is abstract new Limited_Controlled with private;
   type Benchmark_Pointer is access all Benchmark_Type'Class;

   -- Run the benchmark using the specified memory subsystem.
   procedure Run(benchmark : in out Benchmark_Type'Class;
                 mem       : in Memory_Pointer);

   -- Set an argument to the benchmark.
   -- Benchmarks that have additional arguments should override this
   -- procedure and call it only if no benchmark-specific arguments were
   -- recognized.
   procedure Set_Argument(benchmark    : in out Benchmark_Type;
                          arg          : in String);

   -- Destroy the benchmark.
   procedure Destroy(benchmark : in out Benchmark_Pointer);

private

   package Random is new Ada.Numerics.Discrete_Random(Natural);

   package Data_Vectors is new Vectors(Natural, Integer);

   type Benchmark_Type is abstract new Limited_Controlled with record
      generator   : Random.Generator;
      mem         : Memory.Memory_Pointer;
      data        : Data_Vectors.Vector;
      spacing     : Time_Type := 0;
      seed        : Integer   := 15;
   end record;

   -- The body of the benchmark.
   procedure Run(benchmark : in out Benchmark_Type) is null;

   -- Check if argument in arg matches name.
   -- This will '=' and all folloing characters in arg.
   function Check_Argument(arg   : String;
                           name  : String) return Boolean;

   -- Extract the value from an argument (the part following '=').
   function Extract_Argument(arg : String) return String;

   -- Get a random number.
   function Get_Random(benchmark : Benchmark_Type'Class) return Natural;

   -- Simulate a memory read and return the integer stored at the address.
   function Read_Value(benchmark : Benchmark_Type'Class;
                       address   : Natural) return Integer;

   -- Simulate a memory write and set an integer to be stored at the address.
   procedure Write_Value(benchmark  : in out Benchmark_Type'Class;
                         address    : in Natural;
                         value      : in Integer);

   -- Simulate a memory read.
   procedure Read(benchmark   : in Benchmark_Type'Class;
                  address     : in Address_Type;
                  size        : in Positive);

   -- Simulate a memory write.
   procedure Write(benchmark  : in Benchmark_Type'Class;
                   address    : in Address_Type;
                   size       : in Positive);

   -- Simulate idle cycles.
   procedure Idle(benchmark   : in Benchmark_Type'Class;
                  cycles      : in Time_Type);

end Benchmark;
