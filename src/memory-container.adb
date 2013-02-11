
package body Memory.Container is

   procedure Set_Memory(mem   : in out Container_Type'Class;
                        other : access Memory_Type'Class) is
   begin
      mem.mem := other;
   end Set_Memory;

   procedure Start(mem : in out Container_Type) is
   begin
      if mem.mem /= null then
         Start(mem.mem.all);
         mem.time := mem.mem.time;
      end if;
   end Start;

   procedure Commit(mem    : in out Container_Type;
                    cycles : out Time_Type) is
   begin
      if mem.mem /= null then
         Commit(mem.mem.all, cycles);
         mem.time := mem.mem.time;
      else
         cycles := 0;
      end if;
   end Commit;

   procedure Read(mem      : in out Container_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      if mem.mem /= null then
         Read(mem.mem.all, address, size);
         mem.time := mem.mem.time;
      end if;
   end Read;

   procedure Write(mem     : in out Container_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      if mem.mem /= null then
         Write(mem.mem.all, address, size);
         mem.time := mem.mem.time;
      end if;
   end Write;

   procedure Idle(mem      : in out Container_Type;
                  cycles   : in Time_Type) is
   begin
      if mem.mem /= null then
         Idle(mem.mem.all, cycles);
         mem.time := mem.mem.time;
      else
         Advance(mem, cycles);
      end if;
   end Idle;

   procedure Show_Access_Stats(mem : in Container_Type) is
   begin
      if mem.mem /= null then
         Show_Access_Stats(mem.mem.all);
      end if;
   end Show_Access_Stats;

   procedure Finalize(mem : in out Container_Type) is
   begin
      Destroy(Memory_Pointer(mem.mem));
   end Finalize;

end Memory.Container;
