
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Histograms is

   procedure Reset(hist : in out Histogram_Type) is
   begin
      hist.data.Clear;
   end Reset;

   procedure Increment(hist   : in out Histogram_Type;
                       key    : in Key_Type) is
      value : Long_Integer := 1;
      pos   : constant Histogram_Maps.Cursor := hist.data.Find(key);
   begin
      if Histogram_Maps.Has_Element(pos) then
         value := hist.data.Element(key) + 1;
         hist.data.Replace_Element(pos, value);
      else
         hist.data.Insert(key, value);
      end if;
   end Increment;

   function Get_Total(hist : Histogram_Type) return Float is
      total : Float := 0.0;
      procedure Helper(pos : in Histogram_Maps.Cursor) is
      begin
         total := total + Float(Histogram_Maps.Element(pos));
      end Helper;
   begin
      hist.data.Iterate(Helper'Access);
      if total = 0.0 then
         return 1.0;
      else
         return total;
      end if;
   end Get_Total;

   procedure Show(hist     : in Histogram_Type;
                  label    : in String) is

      total : constant Float := Get_Total(hist);

      procedure Helper(pos : in Histogram_Maps.Cursor) is
         key      : constant Key_Type := Histogram_Maps.Key(pos);
         value    : constant Long_Integer := Histogram_Maps.Element(pos);
         percent  : constant Float := 100.0 * Float(value) / total;
      begin
         if percent >= 1.0 then
            Put("    " & Key_Type'Image(key) & ":");
            Put(percent, Fore => 3, Aft => 0, Exp => 0);
            Put("%");
            New_Line;
         end if;
      end Helper;

   begin
      Put_Line(label & ":");
      hist.data.Iterate(Helper'Access);
   end Show;

end Histograms;
