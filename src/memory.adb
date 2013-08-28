
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;

with Device;            use Device;
with Memory.Container;  use Memory.Container;
with Memory.Split;      use Memory.Split;
with Memory.Transform;  use Memory.Transform;

package body Memory is

   next_id : Natural := 0;

   function Done(mem : Memory_Type) return Boolean is
   begin
      return True;
   end Done;

   procedure Reset(mem     : in out Memory_Type;
                   context : in Natural) is
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

   procedure Advance(mem      : in out Memory_Type'Class;
                     cycles   : in Time_Type) is
   begin
      mem.time := mem.time + cycles;
   end Advance;

   function Get_Path_Length(mem : Memory_Type) return Natural is
   begin
      return 0;
   end Get_Path_Length;

   -- Determine the max path length for a memory subsystem.
   function Get_Max_Length(mem      : access Memory_Type'Class;
                           result   : Natural := 0) return Natural is
   begin
      if mem = null then
         return result;
      elsif mem.all in Split_Type'Class then
         declare
            sp : constant Split_Pointer   := Split_Pointer(mem);
            b0 : constant Memory_Pointer  := Get_Bank(sp.all, 0);
            s0 : constant Natural         := Get_Max_Length(b0, result);
            b1 : constant Memory_Pointer  := Get_Bank(sp.all, 1);
            s1 : constant Natural         := Get_Max_Length(b1, result);
            bm : constant Natural         := Natural'Max(s0, s1);
         begin
            return Natural'Max(bm, Get_Path_Length(mem.all));
         end;
      elsif mem.all in Transform_Type'Class then
         declare
            tp : constant Transform_Pointer  := Transform_Pointer(mem);
            bp : constant Memory_Pointer     := Get_Bank(tp.all);
            bs : constant Natural            := Get_Max_Length(bp, result);
         begin
            return Natural'Max(bs, Get_Path_Length(mem.all));
         end;
      elsif mem.all in Container_Type'Class then
         declare
            cp : constant Container_Pointer  := Container_Pointer(mem);
            np : constant Memory_Pointer     := Get_Memory(cp.all);
            a  : constant Natural := Get_Max_Length(np, result);
         begin
            return Natural'Max(a, Get_Path_Length(mem.all));
         end;
      else
         return Natural'Max(result, Get_Path_Length(mem.all));
      end if;
   end Get_Max_Length;

   procedure Deallocate is
      new Ada.Unchecked_Deallocation(Memory_Type'Class, Memory_Pointer);

   procedure Destroy(mem : in out Memory_Pointer) is
   begin
      Deallocate(mem);
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

   function Get_Port(mem : Memory_Type'Class) return Port_Type is
      result : Port_Type;
   begin
      result.id         := Get_ID(mem);
      result.word_size  := Get_Word_Size(mem);
      result.addr_bits  := Get_Address_Bits;
      return result;
   end Get_Port;

   procedure Line(dest  : in out Unbounded_String;
                  str   : in String := "") is
   begin
      Append(dest, str);
      Append(dest, Ada.Characters.Latin_1.LF);
   end Line;

   procedure Signal(sigs   : in out Unbounded_String;
                    name   : in String;
                    width  : in String) is
      rstr : constant String := width & " downto 0";
   begin
      Line(sigs, "   signal " & name & " : std_logic_vector(" & rstr & ");");
   end Signal;

   procedure Signal(sigs   : in out Unbounded_String;
                    name   : in String) is
   begin
      Line(sigs, "   signal " & name & " : std_logic;");
   end Signal;

   procedure Declare_Signals(sigs      : in out Unbounded_String;
                             name      : in String;
                             word_bits : in Positive) is
   begin
      Signal(sigs, name & "_addr", "ADDR_WIDTH - 1");
      Signal(sigs, name & "_din", To_String(word_bits - 1));
      Signal(sigs, name & "_dout", To_String(word_bits - 1));
      Signal(sigs, name & "_re");
      Signal(sigs, name & "_we");
      Signal(sigs, name & "_mask", To_String(word_bits / 8 - 1));
      Signal(sigs, name & "_ready");
   end Declare_Signals;

   function To_String(a : Address_Type) return String is
      str : constant String := Address_Type'Image(a);
   begin
      if str(str'First) = ' ' then
         return str(str'First + 1 .. str'Last);
      else
         return str;
      end if;
   end To_String;

   function To_String(t : Time_Type) return String is
      str : constant String := Time_Type'Image(t);
   begin
      if str(str'First) = ' ' then
         return str(str'First + 1 .. str'Last);
      else
         return str;
      end if;
   end To_String;

end Memory;
