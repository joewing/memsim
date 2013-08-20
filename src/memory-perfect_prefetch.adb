
with Ada.Assertions; use Ada.Assertions;

package body Memory.Perfect_Prefetch is

   function Create_Perfect_Prefetch(mem   : access Memory_Type'Class)
                                    return Perfect_Prefetch_Pointer is
      result : constant Perfect_Prefetch_Pointer := new Perfect_Prefetch_Type;
   begin
      Set_Memory(result.all, mem);
      return result;
   end Create_Perfect_Prefetch;

   function Clone(mem : Perfect_Prefetch_Type) return Memory_Pointer is
      result : constant Perfect_Prefetch_Pointer :=
               new Perfect_Prefetch_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Reset(mem     : in out Perfect_Prefetch_Type;
                   context : in Natural) is
   begin
      Reset(Container_Type(mem), context);
      mem.pending := 0;
   end Reset;

   procedure Read(mem      : in out Perfect_Prefetch_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin

      if mem.pending > 0 then
         Advance(mem, mem.pending);
         mem.pending := 0;
      end if;

      Start(mem);
      Do_Read(mem, address, size);
      Commit(mem, mem.pending);

   end Read;

   procedure Write(mem     : in out Perfect_Prefetch_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin

      Write(Container_Type(mem), address, size);
      Advance(mem, mem.pending);
      mem.pending := 0;

   end Write;

   procedure Idle(mem      : in out Perfect_Prefetch_Type;
                  cycles   : in Time_Type) is
   begin
      if cycles > mem.pending then
         mem.pending := 0;
      else
         mem.pending := mem.pending - cycles;
      end if;
      Advance(mem, cycles);
   end Idle;

   function To_String(mem : Perfect_Prefetch_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(perfect ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Perfect_Prefetch_Type) return Cost_Type is
   begin
      return Get_Cost(Container_Type(mem));
   end Get_Cost;

   procedure Generate(mem  : in Perfect_Prefetch_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
   begin
      Assert(False, "Memory.Perfect_Prefetch.Generate not implemented");
   end Generate;

end Memory.Perfect_Prefetch;
