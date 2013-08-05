
with Memory.Container; use Memory.Container;

package Memory.Register is

   type Register_Type is new Container_Type with private;

   type Register_Pointer is access all Register_Type'Class;

   procedure Insert_Registers(mem : access Memory_Type'Class);

   function Remove_Registers(mem : Memory_Pointer) return Memory_Pointer;

   function Create_Register(mem : access Memory_Type'Class)
                            return Register_Pointer;

   overriding
   function Clone(mem : Register_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Register_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   procedure Read(mem      : in out Register_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Register_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   function Get_Path_Length(mem : Register_Type) return Natural;

   overriding
   function To_String(mem : Register_Type) return Unbounded_String;

   overriding
   procedure Generate(mem  : in Register_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

private

   type Register_Type is new Container_Type with null record;

end Memory.Register;
