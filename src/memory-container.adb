
with Ada.Assertions; use Ada.Assertions;

package body Memory.Container is

   function Get_Memory(mem : Container_Type'Class) return Memory_Pointer is
   begin
      return Memory_Pointer(mem.mem);
   end Get_Memory;

   procedure Set_Memory(mem   : in out Container_Type'Class;
                        other : access Memory_Type'Class) is
   begin
      mem.mem := other;
   end Set_Memory;

   function Done(mem : Container_Type) return Boolean is
   begin
      if mem.mem /= null then
         return Done(mem.mem.all);
      else
         return True;
      end if;
   end Done;

   procedure Reset(mem     : in out Container_Type;
                   context : in Natural) is
   begin
      Reset(Memory_Type(mem), context);
      if mem.mem /= null then
         Reset(mem.mem.all, context);
      end if;
      mem.start_time := 0;
   end Reset;

   procedure Set_Port(mem     : in out Container_Type;
                      port    : in Natural;
                      ready   : out Boolean) is
   begin
      if mem.mem /= null then
         Set_Port(mem.mem.all, port, ready);
      end if;
   end Set_Port;

   procedure Read(mem      : in out Container_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      cycles : Time_Type;
   begin
      if mem.mem /= null then
         Start(mem);
         Read(mem.mem.all, address, size);
         Commit(mem, cycles);
         Advance(mem, cycles);
      end if;
   end Read;

   procedure Write(mem     : in out Container_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      cycles : Time_Type;
   begin
      if mem.mem /= null then
         Start(mem);
         Write(mem.mem.all, address, size);
         Commit(mem, cycles);
         Advance(mem, cycles);
      end if;
   end Write;

   procedure Idle(mem      : in out Container_Type;
                  cycles   : in Time_Type) is
   begin
      if mem.mem /= null then
         Idle(mem.mem.all, cycles);
      end if;
      Advance(mem, cycles);
   end Idle;

   procedure Start(mem : in out Container_Type'Class) is
   begin
      if mem.mem /= null then
         mem.start_time := Get_Time(mem.mem.all);
      else
         mem.start_time := mem.time;
      end if;
   end Start;

   procedure Commit(mem    : in out Container_Type'Class;
                    cycles : out Time_Type) is
   begin
      if mem.mem /= null then
         cycles := Get_Time(mem.mem.all) - mem.start_time;
      else
         cycles := mem.time - mem.start_time;
      end if;
   end Commit;

   procedure Do_Read(mem      : in out Container_Type'Class;
                     address  : in Address_Type;
                     size     : in Positive) is
   begin
      if mem.mem /= null then
         Read(mem.mem.all, address, size);
      end if;
   end Do_Read;

   procedure Do_Write(mem     : in out Container_Type'Class;
                      address : in Address_Type;
                      size    : in Positive) is
   begin
      if mem.mem /= null then
         Write(mem.mem.all, address, size);
      end if;
   end Do_Write;

   procedure Do_Idle(mem      : in out Container_Type'Class;
                     cycles   : in Time_Type) is
   begin
      if mem.mem /= null then
         Idle(mem.mem.all, cycles);
      else
         Advance(mem, cycles);
      end if;
   end Do_Idle;

   function Get_Path_Length(mem : Container_Type) return Natural is
   begin
      if mem.mem /= null then
         return Get_Path_Length(mem.mem.all);
      else
         return 0;
      end if;
   end Get_Path_Length;

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

   function Get_Word_Size(mem : Container_Type) return Positive is
   begin
      Assert(mem.mem /= null, "null memory in Memory.Container.Get_Word_Size");
      return Get_Word_Size(mem.mem.all);
   end Get_Word_Size;

   function Get_Ports(mem : Container_Type) return Port_Vector_Type is
   begin
      return Get_Ports(mem.mem.all);
   end Get_Ports;

   procedure Adjust(mem : in out Container_Type) is
   begin
      if mem.mem /= null then
         mem.mem := Clone(mem.mem.all);
      end if;
   end Adjust;

   procedure Finalize(mem : in out Container_Type) is
   begin
      Destroy(Memory_Pointer(mem.mem));
      Finalize(Memory_Type(mem));
   end Finalize;

end Memory.Container;
