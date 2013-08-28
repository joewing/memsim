
-- Package to provide device-specific information.
package Device is

   -- Supported devices.
   type Device_Type is (
      ASIC,
      Virtex_4,
      Virtex_5,
      Virtex_6,
      Virtex_7
   );

   Invalid_Device : exception;

   -- Set the device to use.
   procedure Set_Device(name : in String);

   -- Set the number of address bits to use.
   procedure Set_Address_Bits(b : in Positive);

   -- Get the current device.
   function Get_Device return Device_Type;

   -- Get the width of a BRAM in bits.
   function Get_BRAM_Width return Positive;

   -- Get the depth of a BRAM in entries.
   function Get_BRAM_Depth return Positive;

   -- Get the maximum number of logic levels allowed.
   function Get_Max_Path return Positive;

   -- Get the number of address bits.
   function Get_Address_Bits return Positive;

end Device;
