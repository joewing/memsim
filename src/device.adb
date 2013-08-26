
package body Device is

   type Device_Property_Type is record
      bram_width  : Natural;
      bram_depth  : Natural;
      max_path    : Natural;
   end record;

   type Device_Property_Array_Type is
      array (Device_Type) of Device_Property_Type;

   properties : constant Device_Property_Array_Type := (
      ASIC        => (1, 1, 64),
      Virtex_4    => (36, 512, 64),
      Virtex_5    => (72, 512, 64),
      Virtex_6    => (72, 512, 64),
      Virtex_7    => (72, 512, 64)
   );

--   device : Device_Type := Virtex_7;
   device : Device_Type := ASIC;

   procedure Set_Device(d : in Device_Type) is
   begin
      device := d;
   end Set_Device;

   function Get_Device return Device_Type is
   begin
      return device;
   end Get_Device;

   function Get_BRAM_Width return Natural is
   begin
      return properties(device).bram_width;
   end Get_BRAM_Width;

   function Get_BRAM_Depth return Natural is
   begin
      return properties(device).bram_depth;
   end Get_BRAM_Depth;

   function Get_Max_Path return Natural is
   begin
      return properties(device).max_path;
   end Get_Max_Path;

end Device;
