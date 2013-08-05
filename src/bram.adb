
with Util; use Util;
with Device;

package body BRAM is

   function Get_Count(width : Natural;
                      depth : Natural) return Natural is
      bram_width  : constant Natural := Device.Get_BRAM_Width;
      bram_depth  : constant Natural := Device.Get_BRAM_Depth;
      result      : Natural := 0;
   begin

      -- Handle the BRAM that is less than bram_width wide (if there is one).
      if (width mod bram_width) /= 0 then
         declare
            max_width      : constant Natural := bram_width * bram_depth;
            small_width    : constant Natural := width mod bram_width;
            rounded_width  : constant Natural := Round_Power2(small_width);
            small_depth    : constant Natural := max_width / rounded_width;
         begin
            result := (depth + small_depth - 1) / small_depth;
         end;
      end if;

      -- Handle BRAMs that are bram_width wide.
      declare
         big_count : constant Natural := width / bram_width;
         big_depth : constant Natural := (depth + bram_depth - 1) / bram_depth;
      begin
         result := result + big_depth * big_count;
      end;

      return result;

   end Get_Count;

end BRAM;
