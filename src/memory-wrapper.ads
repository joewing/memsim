
with Memory.Container; use Memory.Container;

package Memory.Wrapper is

   type Wrapper_Type is abstract new Container_Type with private;

   type Wrapper_Pointer is access all Wrapper_Type'Class;

   procedure Forward_Read(mem       : in out Wrapper_Type;
                          source    : in Natural;
                          address   : in Address_Type;
                          size      : in Positive) is abstract;

   procedure Forward_Write(mem      : in out Wrapper_Type;
                           source   : in Natural;
                           address  : in Address_Type;
                           size     : in Positive) is abstract;

   procedure Forward_Idle(mem       : in out Wrapper_Type;
                          source    : in Natural;
                          cycles    : in Time_Type) is abstract;

   function Forward_Get_Time(mem : Wrapper_Type) return Time_Type is abstract;

   function Get_Join_Length(mem : Wrapper_Type) return Natural is abstract;

private

   type Wrapper_Type is abstract new Container_Type with null record;

end Memory.Wrapper;
