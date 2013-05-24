
with Memory.Split; use Memory.Split;

package Memory.Join is

   type Join_Type is new Memory_Type with private;

   type Join_Pointer is access all Join_Type'Class;

   function Create_Join return Join_Pointer;

   overriding
   procedure Set_Split(mem    : in out Join_Type;
                       index  : in Natural;
                       other  : in Memory_Pointer);

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
   procedure Adjust(mem : in out Join_Type);

private

   type Join_Type is new Memory_Type with record
      index : Natural         := 0;
      split : Split_Pointer   := null;
   end record;

end Memory.Join;
