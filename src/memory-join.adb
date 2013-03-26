
with Ada.Assertions; use Ada.Assertions;

with Memory.Container; use Memory.Container;

package body Memory.Join is

   function Create_Join return Join_Pointer is
      result : constant Join_Pointer := new Join_Type;
   begin
      return result;
   end Create_Join;

   procedure Set_Split(mem    : in out Join_Type;
                       other  : access Memory_Type'Class) is
   begin
      mem.split := Split_Pointer(other);
   end Set_Split;

   function Clone(mem : Join_Type) return Memory_Pointer is
      result : constant Join_Pointer := new Join_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Read(mem      : in out Join_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      if mem.split /= null then
         Forward_Read(mem.split.all, address, size);
      end if;
   end Read;

   procedure Write(mem     : in out Join_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      if mem.split /= null then
         Forward_Write(mem.split.all, address, size);
      end if;
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

   procedure Adjust(mem : in out Join_Type) is
   begin
      mem.split := null;
   end Adjust;

end Memory.Join;
