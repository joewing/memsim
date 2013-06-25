
with Util; use Util;

package Applicative is

   type Applicative_Type is limited interface;

   type Applicative_Pointer is access all Applicative_Type'Class;

   function Apply(app   : Applicative_Type;
                  addr  : Address_Type;
                  dir   : Boolean) return Address_Type is abstract;

end Applicative;
