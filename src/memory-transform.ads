
with Memory.Container; use Memory.Container;

package Memory.Transform is

   type Transform_Type is abstract new Container_Type with private;

   overriding
   procedure Read(mem      : in out Transform_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Transform_Type;
                   address : in Address_Type;
                   size    : in Positive);

   function Apply(mem      : Transform_Type;
                  address  : Address_Type)
                  return Address_Type is abstract;

private

   type Transform_Type is abstract new Container_Type with null record;

end Memory.Transform;
