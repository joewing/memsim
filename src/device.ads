
package Device is

   type Device_Type is (
      Virtex_4,
      Virtex_5,
      Virtex_6,
      Virtex_7
   );

   procedure Set_Device(d : in Device_Type);

   function Get_Device return Device_Type;

   function Get_BRAM_Width return Natural;

   function Get_BRAM_Depth return Natural;

   function Get_Max_Path return Natural;

end Device;
