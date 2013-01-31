
with Clock; use Clock;

package Memory is

   type Address_Type is mod 2 ** 32;

   type Memory_Type is abstract tagged limited private;

   type Memory_Pointer is access all Memory_Type;

   function Read(mem      : Memory_Pointer;
                 address  : Address_Type) return Natural is abstract;

   function Write(mem     : Memory_Pointer;
                  address : Address_Type) return Natural is abstract;

private

   type Memory_Type is abstract tagged limited record
      clock : Clock_Pointer;
   end record;

end Memory;

