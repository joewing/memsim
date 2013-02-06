
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.Dup is

   function Create_Dup return Dup_Pointer is
   begin
      return new Dup_Type;
   end Create_Dup;

   procedure Add_Memory(mem   : in out Dup_Type;
                        other : access Memory_Type'Class) is
   begin
      mem.memories.Append(Memory_Pointer(other));
   end Add_Memory;

   procedure Update_Time(mem : in out Dup_Type) is
      max_time : Time_Type := 0;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            t : constant Time_Type := mem.memories.Element(i).time;
         begin
            max_time := max_time + t;
         end;
      end loop;
      mem.time := max_time;
   end Update_Time;

   procedure Start(mem : in out Dup_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Start(mem.memories.Element(i).all);
      end loop;
   end Start;

   procedure Commit(mem    : in out Dup_Type;
                    cycles : out Time_Type) is
      total : Time_Type := 0;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            temp : Time_Type;
         begin
            Commit(mem.memories.Element(i).all, temp);
            total := total + temp;
         end;
      end loop;
      cycles := total;
   end Commit;

   procedure Read(mem      : in out Dup_Type;
                  address  : in Address_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Read(mem.memories.Element(i).all, address);
      end loop;
      Update_Time(mem);
   end Read;

   procedure Write(mem     : in out Dup_Type;
                   address : in Address_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Write(mem.memories.Element(i).all, address);
      end loop;
      Update_Time(mem);
   end Write;

   procedure Idle(mem      : in out Dup_Type;
                  cycles   : in Time_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Idle(mem.memories.Element(i).all, cycles);
      end loop;
      Update_Time(mem);
   end Idle;

   procedure Show_Stats(mem : in Dup_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            other : constant Memory_Pointer := mem.memories.Element(i);
         begin
            Put(Integer'Image(i) & ": ");
            Show_Stats(other.all);
            Show_Access_Stats(other.all);
         end;
      end loop;
   end Show_Stats;

   procedure Finalize(mem : in out Dup_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            ptr : Memory_Pointer := mem.memories.Element(i);
         begin
            Destroy(ptr);
         end;
      end loop;
   end Finalize;

end Memory.Dup;
