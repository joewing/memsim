
with Histograms;

package Memory.Stats is

   type Stats_Type is new Memory_Type with private;

   type Stats_Pointer is access all Stats_Type'Class;

   function Create_Stats(mem : access Memory_Type'Class) return Stats_Pointer;

   overriding
   procedure Read(mem      : in out Stats_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Stats_Type;
                   address : in Address_Type;
                   size    : in Positive);

private

   package Address_Histograms is new Histograms(Address_Type);
   package Stride_Histograms is new Histograms(Integer);

   type Stats_Type is new Memory_Type with record
      mem            : access Memory_Type'Class := null;
      last_address   : Address_Type := 0;
      last_stride    : Integer := 0;
      reads          : Long_Integer := 0;
      writes         : Long_Integer := 0;
      addresses      : Address_Histograms.Histogram_Type;
      strides        : Stride_Histograms.Histogram_Type;
      multipliers    : Stride_Histograms.Histogram_Type;
   end record;

   overriding
   procedure Show_Access_Stats(mem : in Stats_Type);

   overriding
   procedure Finalize(mem : in out Stats_Type);

end Memory.Stats;
