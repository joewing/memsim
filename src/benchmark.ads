
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Memory;

use Ada.Containers;
use Ada.Finalization;
use Memory;

package Benchmark is

   type Benchmark_Type is abstract new Limited_Controlled with private;

   type Benchmark_Pointer is access all Benchmark_Type'Class;

   Invalid_Argument : exception;

   procedure Run(benchmark : in out Benchmark_Type'Class;
                 mem       : in Memory_Pointer);

   procedure Set_Argument(benchmark    : in out Benchmark_Type;
                          arg          : in String);

   procedure Run(benchmark : in out Benchmark_Type) is abstract;

   procedure Destroy(benchmark : in out Benchmark_Pointer);

private

   package Random is new Ada.Numerics.Discrete_Random(Natural);

   package Data_Vectors is new Vectors(Natural, Integer);

   type Benchmark_Type is abstract new Limited_Controlled with record
      generator   : Random.Generator;
      mem         : Memory.Memory_Pointer;
      data        : Data_Vectors.Vector;
   end record;

   function Get_Random(benchmark : Benchmark_Type'Class) return Natural;

   function Read(benchmark : Benchmark_Type'Class;
                 address   : Natural) return Integer;

   procedure Write(benchmark  : in out Benchmark_Type'Class;
                   address    : in Natural;
                   value      : in Integer);

   procedure Idle(benchmark   : in out Benchmark_Type'Class;
                  cycles      : in Time_Type);

end Benchmark;
