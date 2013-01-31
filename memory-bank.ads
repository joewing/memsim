
with Ada.Containers.Vectors; use Ada.Containers;

package Memory.Bank is

   type Bank_Type is new Memory_Type with private;

   type Bank_Pointer is access Bank_Type;

   function Create_Bank(clock : Clock_Pointer) return Bank_Pointer;

   function Read(mem      : Bank_Pointer;
                 address  : Address_Type) return Natural;

   function Write(mem     : Bank_Pointer;
                  address : Address_Type) return Natural;

   procedure Add_Bank(mem  : Bank_Pointer;
                      bank : Memory_Pointer;
                      key  : Address_Type;
                      mask : Address_Type);

private

   type Bank_Data is record
      mem      : Memory_Pointer;
      key      : Address_Type;
      mask     : Address_Type;
   end record;

   package Bank_Vectors is new Vectors(Natural, Bank_Data);

   type Bank_Type is new Memory_Type with record
      banks    : Bank_Vectors.Vector;
   end record;

end Memory.Bank;

