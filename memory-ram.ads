
package Memory.RAM is

   type RAM_Type is new Memory_Type with private;

   type RAM_Pointer is access RAM_Type;

   function Create_RAM(clock     : Clock_Pointer;
                       latency   : Natural := 1) return RAM_Pointer;

   function Read(mem      : RAM_Pointer;
                 address  : Address_Type) return Natural;

   function Write(mem     : RAM_Pointer;
                  address : Address_Type) return Natural;

   procedure Set_Latency(mem     : access RAM;
                         latency : Natural);

private

   type RAM is new Memory_Base with record
      latency  : Natural := 1;
   end record;

end Memory.RAM;
