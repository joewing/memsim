
with Memory.Container;  use Memory.Container;
with Memory.Wrapper;    use Memory.Wrapper;

package Memory.Transform is

   type Transform_Type is abstract new Container_Type
      and Wrapper_Type with private;

   type Transform_Pointer is access all Transform_Type'Class;

   function Get_Name(mem : Transform_Type) return String;

   function Get_Bank(mem : Transform_Type) return Memory_Pointer;

   procedure Set_Bank(mem  : in out Transform_Type;
                      bank : access Memory_Type'Class);

   procedure Set_Value(mem    : in out Transform_Type;
                       value  : in Integer);

   overriding
   procedure Reset(mem : in out Transform_Type);

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
   procedure Generate(mem  : in Transform_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   procedure Generate_Simple(mem    : in Transform_Type;
                             sigs   : in out Unbounded_String;
                             code   : in out Unbounded_String) is abstract;

   procedure Generate_Banked(mem    : in Transform_Type;
                             sigs   : in out Unbounded_String;
                             code   : in out Unbounded_String) is abstract;

   function Is_Empty(mem : Transform_Type) return Boolean;

   overriding
   procedure Adjust(mem : in out Transform_Type);

   overriding
   procedure Finalize(mem : in out Transform_Type);

private

   type Transform_Type is abstract new Container_Type and Wrapper_Type with
   record
      name  : Unbounded_String;
      bank  : access Memory_Type'Class := null;
      value : Integer := 0;
   end record;

   function Get_Alignment(mem : Transform_Type) return Positive;

end Memory.Transform;
