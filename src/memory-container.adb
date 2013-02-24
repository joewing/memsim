
package body Memory.Container is

   procedure Set_Memory(mem   : in out Container_Type'Class;
                        other : access Memory_Type'Class) is
   begin
      mem.mem := other;
   end Set_Memory;

   function Get_Memory(mem : Container_Type'Class) return Memory_Pointer is
   begin
      return Memory_Pointer(mem.mem);
   end Get_Memory;

   procedure Reset(mem : in out Container_Type) is
   begin
      Reset(Memory_Type(mem));
      if mem.mem /= null then
         Reset(mem.mem.all);
      end if;
   end Reset;

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

   procedure Forward_Start(mem : in out Container_Type'Class) is
   begin
      if mem.mem /= null then
         Start(mem.mem.all);
      end if;
   end Forward_Start;

   procedure Forward_Commit(mem     : in out Container_Type'Class;
                            cycles  : out Time_Type) is
   begin
      if mem.mem /= null then
         Commit(mem.mem.all, cycles);
      else
         cycles := 0;
      end if;
   end Forward_Commit;

   procedure Forward_Read(mem       : in out Container_Type'Class;
                          address   : in Address_Type;
                          size      : in Positive) is
   begin
      if mem.mem /= null then
         Read(mem.mem.all, address, size);
      end if;
   end Forward_Read;

   procedure Forward_Write(mem      : in out Container_Type'Class;
                           address  : in Address_Type;
                           size     : in Positive) is
   begin
      if mem.mem /= null then
         Write(mem.mem.all, address, size);
      end if;
   end Forward_Write;

   procedure Forward_Idle(mem    : in out Container_Type'Class;
                          cycles : in Time_Type) is
   begin
      if mem.mem /= null then
         Idle(mem.mem.all, cycles);
      else
         Advance(mem, cycles);
      end if;
   end Forward_Idle;

   procedure Show_Access_Stats(mem : in out Container_Type) is
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

   function Get_Cost(mem : Container_Type) return Cost_Type is
   begin
      if mem.mem /= null then
         return Get_Cost(mem.mem.all);
      else
         return 0;
      end if;
   end Get_Cost;

   function Get_Writes(mem : Container_Type) return Long_Integer is
   begin
      if mem.mem /= null then
         return Get_Writes(mem.mem.all);
      else
         return 0;
      end if;
   end Get_Writes;

   procedure Adjust(mem : in out Container_Type) is
   begin
      if mem.mem /= null then
         mem.mem := Clone(mem.mem.all);
      end if;
   end Adjust;

   procedure Finalize(mem : in out Container_Type) is
   begin
      if mem.mem /= null then
         Destroy(Memory_Pointer(mem.mem));
      end if;
   end Finalize;

end Memory.Container;
