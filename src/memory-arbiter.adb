
with Ada.Assertions; use Ada.Assertions;

package body Memory.Arbiter is

   function Create_Arbiter(next : access Memory_Type'Class)
                           return Arbiter_Pointer is
      result : constant Arbiter_Pointer := new Arbiter_Type;
   begin
      Set_Memory(result.all, next);
      return result;
   end Create_Arbiter;

   function Clone(mem : Arbiter_Type) return Memory_Pointer is
      result : constant Arbiter_Pointer := new Arbiter_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Reset(mem     : in out Arbiter_Type;
                   context : in Natural) is
   begin
      Reset(Container_Type(mem), context);
   end Reset;

   procedure Set_Port(mem     : in out Arbiter_Type;
                      port    : in Natural;
                      ready   : out Boolean) is
   begin
      mem.port := port;
      ready := True;
   end Set_Port;

   function Get_Next_Time(mem    : Arbiter_Type) return Time_Type is
   begin
      if mem.port > mem.pending.Last_Index then
         return 0;
      else
         return mem.pending.Element(mem.port);
      end if;
   end Get_Next_Time;

   procedure Read(mem      : in out Arbiter_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Read(Container_Type(mem), address, size);
   end Read;

   procedure Write(mem     : in out Arbiter_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Write(Container_Type(mem), address, size);
   end Write;

   procedure Idle(mem      : in out Arbiter_Type;
                  cycles   : in Time_Type) is
   begin
      Assert(False, "Memory.Arbiter.Idle not implemented");
   end Idle;

   function To_String(mem : Arbiter_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(arbiter ");
      Append(result, "(memory ");
      Append(result, To_String(Get_Memory(mem).all));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

end Memory.Arbiter;
