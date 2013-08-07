
with Memory.Wrapper; use Memory.Wrapper;

package Memory.Join is

   type Join_Type is new Memory_Type with private;

   type Join_Pointer is access all Join_Type'Class;

   function Create_Join(parent   : access Wrapper_Type'Class;
                        index    : Natural) return Join_Pointer;

   overriding
   function Clone(mem : Join_Type) return Memory_Pointer;

   overriding
   procedure Read(mem      : in out Join_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Join_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   function Get_Writes(mem : Join_Type) return Long_Integer;

   overriding
   function To_String(mem : Join_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Join_Type) return Cost_Type;

   overriding
   function Get_Word_Size(mem : Join_Type) return Positive;

   overriding
   function Get_Ports(mem : Join_Type) return Port_Vector_Type;

   overriding
   procedure Generate(mem  : in Join_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   overriding
   function Get_Path_Length(mem : Join_Type) return Natural;

   overriding
   procedure Adjust(mem : in out Join_Type);

   procedure Set_Parent(mem      : in out Join_Type;
                        parent   : access Wrapper_Type'Class);

   function Find_Join(mem : Memory_Pointer) return Join_Pointer;

private

   type Join_Type is new Memory_Type with record
      parent   : access Wrapper_Type'Class   := null;
      index    : Natural                     := 0;
   end record;

end Memory.Join;
