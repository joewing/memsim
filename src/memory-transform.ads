
with Memory.Container;  use Memory.Container;
with Memory.Wrapper;    use Memory.Wrapper;

package Memory.Transform is

   type Transform_Type is abstract new Container_Type
      and Wrapper_Type with private;

   type Transform_Pointer is access all Transform_Type'Class;

   function Get_Bank(mem : Transform_Type) return Memory_Pointer;

   procedure Set_Bank(mem  : in out Transform_Type;
                      bank : access Memory_Type'Class);

   overriding
   procedure Read(mem      : in out Transform_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Transform_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   function Get_Cost(mem : Transform_Type) return Cost_Type;

   function Apply(mem      : Transform_Type;
                  address  : Address_Type;
                  dir      : Boolean)
                  return Address_Type is abstract;

   overriding
   procedure Forward_Read(mem       : in out Transform_Type;
                          source    : in Natural;
                          address   : in Address_Type;
                          size      : in Positive);

   overriding
   procedure Forward_Write(mem      : in out Transform_Type;
                           source   : in Natural;
                           address  : in Address_Type;
                           size     : in Positive);

   overriding
   procedure Forward_Idle(mem    : in out Transform_Type;
                          source : in Natural;
                          cycles : in Time_Type);

   overriding
   function Forward_Get_Time(mem : Transform_Type) return Time_Type;

   overriding
   procedure Adjust(mem : in out Transform_Type);

   overriding
   procedure Finalize(mem : in out Transform_Type);

private

   type Transform_Type is abstract new Container_Type and Wrapper_Type with
   record
      bank : access Memory_Type'Class := null;
   end record;

end Memory.Transform;
