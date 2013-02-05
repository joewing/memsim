
with Ada.Text_IO; use Ada.Text_IO;

package body Histograms is

   procedure Increment(hist   : in out Histogram_Type;
                       key    : in Key_Type) is
      value : Natural := 1;
   begin
      if hist.data.Contains(key) then
         value := hist.data.Element(key) + 1;
         hist.data.Replace(key, value);
      else
         hist.data.Insert(key, value);
      end if;
   end Increment;

   procedure Show(hist     : in Histogram_Type;
                  label    : in String) is

      procedure Helper(pos : in Histogram_Maps.Cursor) is
         key   : constant Key_Type := Histogram_Maps.Key(pos);
         value : constant Natural  := Histogram_Maps.Element(pos);
      begin
         Put_Line("  " & Key_Type'Image(key) & ":" & Natural'Image(value));
      end Helper;

   begin
      Put_Line(label & ":");
      hist.data.Iterate(Helper'Access);
   end Show;

end Histograms;
