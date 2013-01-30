
package Memory is

   type Address_Type is mod 2 ** 32;

   type Memory_Base is abstract tagged private;

   type Memory_Pointer is access all Memory_Base'class;

   procedure Read(mem      : access Memory_Base;
                  address  : Address_Type) is abstract;

   procedure Write(mem     : access Memory_Base;
                   address : Address_Type) is abstract;

   procedure Step(mem      : access Memory_Base;
                  cycles   : Natural := 1) is abstract;

   function Get_Time(mem : access Memory_Base) return Natural is abstract;

private

   procedure Set_Time(mem  : access Memory_Base;
                      t    : Natural) is abstract;

   type Memory_Base is abstract tagged with null record;

end Memory;

