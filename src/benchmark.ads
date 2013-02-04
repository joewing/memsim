
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
with Ada.Finalization;
with Memory;

use Ada.Containers;
use Ada.Finalization;
use Memory;

package Benchmark is

   type Benchmark_Type is abstract new Limited_Controlled with private;

   procedure Set_Run_Length(benchmark  : in out Benchmark_Type'class;
                            length     : in Positive);

   procedure Set_Size(benchmark  : in out Benchmark_Type'class;
                      size       : in Positive);

   procedure Run(benchmark : in out Benchmark_Type'class;
                 mem       : in Memory_Pointer);

   procedure Run(benchmark : in out Benchmark_Type) is abstract;

private

   package Random is new Ada.Numerics.Discrete_Random(Natural);

   package Data_Vectors is new Vectors(Natural, Integer);

   type Benchmark_Type is abstract new Limited_Controlled with record
      generator   : Random.Generator;
      mem         : Memory.Memory_Pointer;
      data        : Data_Vectors.Vector;
      length      : Positive := 1000;
      size        : Positive := 1024;
   end record;

   function Get_Random(benchmark : Benchmark_Type'class) return Natural;

   function Read(benchmark : Benchmark_Type'class;
                 address   : Natural) return Integer;

   procedure Write(benchmark  : in out Benchmark_Type'class;
                   address    : in Natural;
                   value      : in Integer);

end Benchmark;
