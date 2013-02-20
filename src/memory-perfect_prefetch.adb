
package body Memory.Perfect_Prefetch is

   function Create_Perfect_Prefetch(mem   : access Memory_Type'Class)
                                    return Perfect_Prefetch_Pointer is
      result : constant Perfect_Prefetch_Pointer := new Perfect_Prefetch_Type;
   begin
      result.mem := mem;
      return result;
   end Create_Perfect_Prefetch;

   procedure Read(mem      : in out Perfect_Prefetch_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin

      if mem.pending > 0 then
         Advance(mem, mem.pending);
         mem.pending := 0;
      end if;

      Start(mem.mem.all);
      Read(mem.mem.all, address, size);
      Commit(mem.mem.all, mem.pending);

   end Read;

   procedure Write(mem     : in out Perfect_Prefetch_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      cycles : Time_Type := 0;
   begin

      Start(mem.mem.all);
      Write(mem.mem.all, address, size);
      Commit(mem.mem.all, cycles);

      Advance(mem, mem.pending);
      Advance(mem, cycles);
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

   procedure Show_Access_Stats(mem : in out Perfect_Prefetch_Type) is
   begin
      Show_Access_Stats(mem.mem.all);
   end Show_Access_Stats;

   function To_String(mem : Perfect_Prefetch_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(perfect ");
      Append(result, To_String(mem.mem.all));
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Perfect_Prefetch_Type) return Cost_Type is
   begin
      return Get_Cost(mem.mem.all);
   end Get_Cost;

   procedure Finalize(mem : in out Perfect_Prefetch_Type) is
   begin
      Destroy(Memory_Pointer(mem.mem));
   end Finalize;

end Memory.Perfect_Prefetch;
