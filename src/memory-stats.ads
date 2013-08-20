
with Histograms;
with Memory.Container; use Memory.Container;

package Memory.Stats is

   type Stats_Type is new Container_Type with private;

   type Stats_Pointer is access all Stats_Type'Class;

   function Create_Stats(mem : access Memory_Type'Class) return Stats_Pointer;

   overriding
   function Clone(mem : Stats_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem     : in out Stats_Type;
                   context : in Natural);

   overriding
   procedure Read(mem      : in out Stats_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Stats_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Show_Access_Stats(mem : in out Stats_Type);

private

   package Address_Histograms is new Histograms(Address_Type);
   package Stride_Histograms is new Histograms(Integer);

   type Stats_Type is new Container_Type with record
      last_address   : Address_Type := 0;
      last_stride    : Integer := 0;
      reads          : Long_Integer := 0;
      min_address    : Address_Type := Address_Type'Last;
      max_address    : Address_Type := Address_Type'First;
      addresses      : Address_Histograms.Histogram_Type;
      strides        : Stride_Histograms.Histogram_Type;
      multipliers    : Stride_Histograms.Histogram_Type;
   end record;

end Memory.Stats;
