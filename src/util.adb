
package body Util is

   function Log2(n : Natural) return Natural is
      i  : Natural := n;
      r  : Natural := 0;
   begin
      while i > 0 loop
         r := r + 1;
         i := i / 2;
      end loop;
      return r;
   end Log2;

end Util;
