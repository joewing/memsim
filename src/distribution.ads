
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
use Ada.Containers;

with Applicative; use Applicative;
with Util;        use Util;

package Distribution is

   type Distribution_Type is limited private;

   type Distribution_Pointer is access all Distribution_Type;

   procedure Set_Seed(dist : in out Distribution_Type;
                      seed : in Integer);

   procedure Insert(dist      : in out Distribution_Type;
                    address   : in Address_Type;
                    size      : in Positive);

   procedure Add_Transform(dist  : in out Distribution_Type;
                           trans : in Applicative_Pointer);

   procedure Reset_Transform(dist : in out Distribution_Type);

   function Random_Address(dist        : Distribution_Type;
                           alignment   : Positive) return Address_Type;

   function Random(dist : Distribution_Type) return Natural;

private

   type Range_Type is record
      start    : Address_Type;
      size     : Positive;
   end record;

   package RNG is new Ada.Numerics.Discrete_Random(Natural);

   package Range_Vectors is new Vectors(Positive, Range_Type);

   package Transformation_Vectors is
      new Vectors(Positive, Applicative_Pointer);

   type Distribution_Type is limited record
      generator         : RNG.Generator;
      ranges            : Range_Vectors.Vector;
      transformations   : Transformation_Vectors.Vector;
   end record;

end Distribution;
