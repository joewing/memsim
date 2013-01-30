
with Ada.Containers.Vectors; use Ada.Containers;

package Memory.Bank is

   type Banked_Memory is new Memory_Base with private;

   type Banked_Pointer is access all Banked_Memory'class;

   procedure Read(mem      : access Banked_Memory;
                  address  : Address_Type);

   procedure Write(mem     : access Banked_Memory;
                   address : Address_Type);

   procedure Step(mem      : access Banked_Memory;
                  cycles   : Natural := 1);

   procedure Add_Bank(mem:    access Banked_Memory;
                      bank:   Memory_Pointer;
                      key:    Address_Type;
                      mask:   Address_Type);

private

   type Bank_Data is record
      mem      : Memory_Pointer;
      key      : Address_Type;
      mask     : Address_Type;
   end record;

   package Bank_Vectors is new Vectors(Natural, Bank_Data);

   type Banked_Memory is new Memory_Base with record
      banks    : Bank_Vectors.Vector;
   end record;

end Memory.Bank;

