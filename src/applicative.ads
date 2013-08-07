
with Util; use Util;

-- Package to provide an interface for types providing an "apply" function.
package Applicative is

   -- Interface for types providing an "apply" function.
   type Applicative_Type is limited interface;
   type Applicative_Pointer is access all Applicative_Type'Class;

   function Apply(app   : Applicative_Type;
                  addr  : Address_Type;
                  dir   : Boolean) return Address_Type is abstract;

end Applicative;
