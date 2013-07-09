
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

   function Log2(n : Long_Integer) return Natural is
      i  : Long_Integer := n;
      r  : Natural := 0;
   begin
      while i > 0 loop
         r := r + 1;
         i := i / 2;
      end loop;
      return r;
   end Log2;

   function Round_Power2(n : Natural) return Natural is
   begin
      return 2 ** Log2(n);
   end Round_Power2;

   function To_String(i : Integer) return String is
      str : constant String := Integer'Image(i);
   begin
      if str(str'First) = ' ' then
         return str(str'First + 1 .. str'Last);
      else
         return str;
      end if;
   end To_String;

   function To_String(i : Long_Integer) return String is
      str : constant String := Long_Integer'Image(i);
   begin
      if str(str'First) = ' ' then
         return str(str'First + 1 .. str'Last);
      else
         return str;
      end if;
   end To_String;

end Util;
