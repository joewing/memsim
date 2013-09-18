
package body Memory.Arbiter is

   function Create_Arbiter(next : access Memory_Type'Class)
                           return Arbiter_Pointer is
      result : constant Arbiter_Pointer := new Arbiter_Type;
   begin
      Set_Memory(result.all, next);
      return result;
   end Create_Arbiter;

   function Clone(mem : Arbiter_Type) return Memory_Pointer is
      result : constant Arbiter_Pointer := new Arbiter'(mem);
   begin
      return result;
   end Clone;

   procedure Reset(mem     : in out Arbiter_Type;
                   context : in Natural) is
   begin
      Reset(Container_Type(mem), context);
   end Reset;

   procedure Set_Port(mem  : in out Arbiter_Type;
                      port : in Natural) is
   begin
   end Set_Port;

end Memory.Arbiter;
