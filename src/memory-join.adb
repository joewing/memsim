
with Ada.Assertions; use Ada.Assertions;

with Memory.Container; use Memory.Container;

package body Memory.Join is

   function Create_Join(split : Split_Pointer;
                        index : Natural)  return Join_Pointer is
      result : constant Join_Pointer := new Join_Type;
   begin
      result.split   := split;
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
      base     : constant Memory_Pointer := Get_Memory(mem.split.all);
      cycles   : Time_Type;
   begin
      Assert(mem.split /= null, "Memory.Join.Read: split is null");
      Assert(base /= null, "Memory.Join.Read: base is null");
      cycles := Get_Time(base.all);
      if mem.index = 0 then
         Read(base.all, address, size);
      else
         Read(base.all, address + Get_Offset(mem.split.all), size);
      end if;
      Advance(mem, Get_Time(base.all) - cycles);
   end Read;

   procedure Write(mem     : in out Join_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      base     : constant Memory_Pointer := Get_Memory(mem.split.all);
      cycles   : Time_Type;
   begin
      Assert(mem.split /= null, "Memory.Join.Write: split is null");
      Assert(base /= null, "Memory.Join.Write: base is null");
      cycles := Get_Time(base.all);
      if mem.index = 0 then
         Write(base.all, address, size);
      else
         Write(base.all, address + Get_Offset(mem.split.all), size);
      end if;
      Advance(mem, Get_Time(base.all) - cycles);
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
      return Get_Word_Size(mem.split.all);
   end Get_Word_Size;

   procedure Generate(mem  : in Join_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      name        : constant String := "m" & To_String(Get_ID(mem));
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
   begin
      Declare_Signals(sigs, name, word_bits);
   end Generate;

   procedure Adjust(mem : in out Join_Type) is
   begin
      mem.split := null;
   end Adjust;

   procedure Set_Split(mem    : in out Join_Type;
                       split  : in Split_Pointer) is
   begin
      mem.split := split;
   end Set_Split;

end Memory.Join;
