
package Util is

   type Address_Type is mod 2 ** 64;

   type Time_Type is new Long_Integer range 0 .. Long_Integer'Last;

   type Cost_Type is new Long_Integer range 0 .. Long_Integer'Last;

   function Log2(n : Natural) return Natural;

   function Round_Power2(n : Natural) return Natural;

   function To_String(i : Integer) return String;

   function To_String(i : Long_Integer) return String;

end Util;
