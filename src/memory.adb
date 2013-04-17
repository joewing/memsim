
with Ada.Text_IO; use Ada.Text_IO;

package body Memory is

   next_id : Natural := 0;

   procedure Permute(mem         : in out Memory_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      null;
   end Permute;

   procedure Reset(mem : in out Memory_Type) is
   begin
      mem.time := 0;
   end Reset;

   procedure Idle(mem      : in out Memory_Type;
                  cycles   : in Time_Type) is
   begin
      Advance(mem, cycles);
   end Idle;

   function Get_Time(mem : Memory_Type) return Time_Type is
   begin
      return mem.time;
   end Get_Time;

   procedure Show_Stats(mem : in out Memory_Type) is
   begin
      Put_Line("Time:" & Time_Type'Image(mem.time) & " cycles");
      Put_Line("Cost:" & Cost_Type'Image(Get_Cost(Memory_Type'Class(mem))));
      Show_Access_Stats(Memory_Type'Class(mem));
   end Show_Stats;

   procedure Show_Access_Stats(mem : in out Memory_Type) is
   begin
      null;
   end Show_Access_Stats;

   procedure Advance(mem      : in out Memory_Type'Class;
                     cycles   : in Time_Type) is
   begin
      mem.time := mem.time + cycles;
   end Advance;

   function Log2(n : Natural) return Natural is
      i  : Natural := n;
      r  : Natural := 0;
   begin
      while i > 0 loop
         r := r + 1;
         i := i / 2;
      end loop;
      return r;
   end Log2;

   procedure Deallocate is
      new Ada.Unchecked_Deallocation(Memory_Type'Class, Memory_Pointer);

   procedure Destroy(mem : in out Memory_Pointer) is
   begin
      if mem /= null then
         Deallocate(mem);
      end if;
   end Destroy;

   function Get_Time(mem : access Memory_Type'Class) return Time_Type is
   begin
      return Get_Time(mem.all);
   end Get_Time;

   function Get_Writes(mem : access Memory_Type'Class) return Long_Integer is
   begin
      return Get_Writes(mem.all);
   end Get_Writes;

   function Get_Zero(mem : access Memory_Type'Class) return Natural is
   begin
      return 0;
   end Get_Zero;

   function Get_ID(mem : Memory_Type'Class) return Natural is
   begin
      return mem.id;
   end Get_ID;

   procedure Initialize(mem : in out Memory_Type) is
   begin
      mem.id := next_id;
      next_id := next_id + 1;
   end Initialize;

   procedure Adjust(mem : in out Memory_Type) is
   begin
      mem.id := next_id;
      next_id := next_id + 1;
   end Adjust;

end Memory;
