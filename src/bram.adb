
with Util; use Util;

package body BRAM is

   BRAM_WIDTH  : constant := 36;
   BRAM_DEPTH  : constant := 512;

   function Get_Count(width : Natural;
                      depth : Natural) return Natural is
      result : Natural := 0;
   begin

      -- Handle the BRAM that is less than BRAM_WIDTH wide (if there is one).
      if (width mod BRAM_WIDTH) /= 0 then
         declare
            max_width      : constant := BRAM_WIDTH * BRAM_DEPTH;
            small_width    : constant Natural := width mod BRAM_WIDTH;
            rounded_width  : constant Natural := Round_Power2(small_width);
            small_depth    : constant Natural := max_width / rounded_width;
         begin
            result := (depth + small_depth - 1) / small_depth;
         end;
      end if;

      -- Handle BRAMs that are BRAM_WIDTH wide.
      declare
         big_count : constant Natural := width / BRAM_WIDTH;
         big_depth : constant Natural := (depth + BRAM_DEPTH - 1) / BRAM_DEPTH;
      begin
         result := result + big_depth * big_count;
      end;

      return result;

   end Get_Count;

end BRAM;
