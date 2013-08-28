
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Device is

   type Device_Property_Type is record
      name        : Unbounded_String;
      bram_width  : Positive;
      bram_depth  : Positive;
      max_path    : Positive;
   end record;

   type Device_Property_Array_Type is
      array (Device_Type) of Device_Property_Type;

   properties : constant Device_Property_Array_Type := (
      ASIC        => (To_Unbounded_String("asic"),        1,   1,    64),
      Virtex_4    => (To_Unbounded_String("virtex4"),    36, 512,    64),
      Virtex_5    => (To_Unbounded_String("virtex5"),    72, 512,    64),
      Virtex_6    => (To_Unbounded_String("virtex6"),    72, 512,    64),
      Virtex_7    => (To_Unbounded_String("virtex7"),    72, 512,    64)
   );

   device         : Device_Type  := ASIC;
   address_bits   : Positive     := 32;

   procedure Set_Device(name : in String) is
   begin
      for d in Device_Property_Array_Type'Range loop
         if properties(d).name = name then
            device := d;
            return;
         end if;
      end loop;
      raise Invalid_Device;
   end Set_Device;

   procedure Set_Address_Bits(b : in Positive) is
   begin
      address_bits := b;
   end Set_Address_Bits;

   function Get_Device return Device_Type is
   begin
      return device;
   end Get_Device;

   function Get_BRAM_Width return Positive is
   begin
      return properties(device).bram_width;
   end Get_BRAM_Width;

   function Get_BRAM_Depth return Positive is
   begin
      return properties(device).bram_depth;
   end Get_BRAM_Depth;

   function Get_Max_Path return Positive is
   begin
      return properties(device).max_path;
   end Get_Max_Path;

   function Get_Address_Bits return Positive is
   begin
      return address_bits;
   end Get_Address_Bits;

end Device;
