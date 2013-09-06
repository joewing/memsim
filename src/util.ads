
-- Miscellaneous types and functions.
package Util is

   -- Type to represent a memory address.
   type Address_Type is mod 2 ** 64;

   -- Type to represent access time in cycles.
   type Time_Type is new Long_Integer range 0 .. Long_Integer'Last;

   -- Type to represent the cost of a memory subsystem.
   type Cost_Type is new Long_Integer range 0 .. Long_Integer'Last;

   -- Add two cost types (saturating addition).
   function "+"(a, b : Cost_Type) return Cost_Type;

   -- Compute the log base 2 of a Natural.
   function Log2(n : Natural) return Natural;

   -- Compute the log base 2 of a Long_Integer.
   function Log2(n : Long_Integer) return Natural;

   -- Round n to the next highest power of 2.
   function Round_Power2(n : Natural) return Natural;

   -- Convert an Integer to a string (without spaces).
   function To_String(i : Integer) return String;

   -- Convert a Long_Integer to a string (without spaces).
   function To_String(i : Long_Integer) return String;

   -- Convert a Long_Float to a string (without spaces).
   function To_String(f : Long_Float) return String;

end Util;
