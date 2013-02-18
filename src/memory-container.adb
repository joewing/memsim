
package body Memory.Container is

   function Create_Container(mem : access Memory_Type'Class)
                             return Container_Pointer is
      result : constant Container_Pointer := new Container_Type;
   begin
      result.mem := mem;
      return result;
   end Create_Container;

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

   function To_String(mem : Container_Type) return Unbounded_String is
   begin
      if mem.mem /= null then
         return To_String(mem.mem.all);
      else
         return Null_Unbounded_String;
      end if;
   end To_String;

   procedure Finalize(mem : in out Container_Type) is
   begin
      if mem.mem /= null then
         Destroy(Memory_Pointer(mem.mem));
      end if;
   end Finalize;

end Memory.Container;
