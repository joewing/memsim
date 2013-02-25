
with Ada.Containers.Ordered_Maps;
use Ada.Containers;

generic
   type Key_Type is (<>);
package Histograms is

   type Histogram_Type is tagged private;

   procedure Reset(hist : in out Histogram_Type);

   procedure Increment(hist   : in out Histogram_Type;
                       key    : in Key_Type);

   procedure Show(hist     : in Histogram_Type;
                  label    : in String);

private

   package Histogram_Maps is new Ordered_Maps(Key_Type      => Key_Type,
                                              Element_Type  => Long_Integer);

   type Histogram_Type is tagged record
      data : Histogram_Maps.Map;
   end record;

end Histograms;
