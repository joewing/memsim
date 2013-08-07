
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors;
use Ada.Containers;

with Applicative; use Applicative;
with Util;        use Util;

-- Package containing functions for generating random numbers.
package Distribution is

   -- Type to represent a random distribution.
   type Distribution_Type is limited private;
   type Distribution_Pointer is access all Distribution_Type;

   -- Set the seed used for selecting random addresses.
   procedure Set_Seed(dist : in out Distribution_Type;
                      seed : in Integer);

   -- Insert an address to the pool.
   -- This should be called the first time a trace is executed.
   procedure Insert(dist      : in out Distribution_Type;
                    address   : in Address_Type;
                    size      : in Positive);

   -- Push an address limit on to the stack.
   -- This is done when evaluating a part of a split or scratchpad.
   -- Only addresses within this limit will be selected at random.
   procedure Push_Limit(dist  : in out Distribution_Type;
                        lower : in Address_Type;
                        upper : in Address_Type);

   -- Pop an address limit off of the stack.
   procedure Pop_Limit(dist : in out Distribution_Type);

   -- Push an address transform on to the stack.
   -- This is done when evaluating an address transform or split.
   procedure Push_Transform(dist    : in out Distribution_Type;
                            trans   : in Applicative_Pointer);

   -- Pop an address transform off of the stack.
   procedure Pop_Transform(dist : in out Distribution_Type);

   -- Select a random address from the distribution.
   function Random_Address(dist        : Distribution_Type;
                           alignment   : Positive) return Address_Type;

   -- Select a uniform random number.
   function Random(dist : Distribution_Type) return Natural;

   -- Display address ranges (for debugging).
   procedure Print(dist : in Distribution_Type);

private

   type Range_Type is record
      start    : Address_Type;
      size     : Positive;
   end record;

   type Limit_Type is record
      trans    : Applicative_Pointer   := null;
      lower    : Address_Type          := 0;
      upper    : Address_Type          := 0;
   end record;

   package RNG is new Ada.Numerics.Discrete_Random(Natural);

   package Range_Vectors is new Vectors(Positive, Range_Type);

   package Limit_Vectors is new Vectors(Positive, Limit_Type);

   type Distribution_Type is limited record
      generator         : RNG.Generator;
      ranges            : Range_Vectors.Vector;
      limits            : Limit_Vectors.Vector;
   end record;

end Distribution;
