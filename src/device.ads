
-- Package to provide device-specific information.
package Device is

   -- Supported devices.
   type Device_Type is (
      Virtex_4,
      Virtex_5,
      Virtex_6,
      Virtex_7
   );

   -- Set the device to use.
   procedure Set_Device(d : in Device_Type);

   -- Get the current device.
   function Get_Device return Device_Type;

   -- Get the width of a BRAM in bits.
   function Get_BRAM_Width return Natural;

   -- Get the depth of a BRAM in entries.
   function Get_BRAM_Depth return Natural;

   -- Get the maximum number of logic levels allowed.
   function Get_Max_Path return Natural;

end Device;
