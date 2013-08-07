
with Ada.Assertions;    use Ada.Assertions;
with Memory.Container;  use Memory.Container;

package body Memory.Join is

   function Create_Join(parent   : access Wrapper_Type'Class;
                        index    : Natural) return Join_Pointer is
      result : constant Join_Pointer := new Join_Type;
   begin
      result.parent  := parent;
      result.index   := index;
      return result;
   end Create_Join;

   function Clone(mem : Join_Type) return Memory_Pointer is
      result : constant Join_Pointer := new Join_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Read(mem      : in out Join_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Forward_Read(mem.parent.all, mem.index, address, size);
   end Read;

   procedure Write(mem     : in out Join_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Forward_Write(mem.parent.all, mem.index, address, size);
   end Write;

   function Get_Writes(mem : Join_Type) return Long_Integer is
   begin
      Assert(False, "Memory.Join.Get_Writes not implemented");
      return 0;
   end Get_Writes;

   function To_String(mem : Join_Type) return Unbounded_String is
   begin
      return To_Unbounded_String("(join)");
   end To_String;

   function Get_Cost(mem : Join_Type) return Cost_Type is
   begin
      return 0;
   end Get_Cost;

   function Get_Word_Size(mem : Join_Type) return Positive is
   begin
      return Get_Word_Size(mem.parent.all);
   end Get_Word_Size;

   function Get_Ports(mem : Join_Type) return Port_Vector_Type is
      result : Port_Vector_Type;
   begin
      Assert(False, "Memory.Join.Get_Ports should not be called");
      return result;
   end Get_Ports;

   procedure Generate(mem  : in Join_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      name        : constant String := "m" & To_String(Get_ID(mem));
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
   begin
      Declare_Signals(sigs, name, word_bits);
   end Generate;

   function Get_Path_Length(mem : Join_Type) return Natural is
      next  : constant Memory_Pointer  := Get_Memory(mem.parent.all);
      jl    : constant Natural         := Get_Join_Length(mem.parent.all);
   begin
      return jl + Get_Path_Length(next.all);
   end Get_Path_Length;

   procedure Adjust(mem : in out Join_Type) is
   begin
      Adjust(Memory_Type(mem));
      mem.parent := null;
   end Adjust;

   procedure Set_Parent(mem      : in out Join_Type;
                        parent   : access Wrapper_Type'Class) is
   begin
      mem.parent := parent;
   end Set_Parent;

   function Find_Join(mem : Memory_Pointer) return Join_Pointer is
   begin
      if mem.all in Join_Type'Class then
         return Join_Pointer(mem);
      else
         declare
            cp : constant Container_Pointer := Container_Pointer(mem);
         begin
            return Find_Join(Get_Memory(cp.all));
         end;
      end if;
   end Find_Join;

end Memory.Join;
