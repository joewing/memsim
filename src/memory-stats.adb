
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.Stats is

   Max_Stride : constant := Integer'Last;

   function Create_Stats(mem : access Memory_Type'Class)
                        return Stats_Pointer is
      result : constant Stats_Pointer := new Stats_Type;
   begin
      result.mem := mem;
      return result;
   end Create_Stats;

   function Compute_Multiple(last, current : Integer) return Integer is
   begin
      if last /= 0 then
         return current / last;
      else
         return 0;
      end if;
   end Compute_Multiple;

   function Compute_Stride(last, current : Address_Type) return Integer is
      stride : Integer := 0;
   begin
      if    last < current and current - last <= Max_Stride then
         stride := Integer(current - last);
      elsif last > current and last - current <= Max_Stride then
         stride := -Integer(last - current);
      end if;
      return stride;
   end Compute_Stride;

   procedure Process(mem      : in out Stats_Type;
                     address  : in Address_Type) is
      stride : constant Integer := Compute_Stride(mem.last_address, address);
      mult   : constant Integer := Compute_Multiple(mem.last_stride, stride);
   begin
      mem.addresses.Increment(address);
      mem.strides.Increment(stride);
      mem.multipliers.Increment(mult);
      mem.last_address  := address;
      mem.last_stride   := stride;
   end Process;

   procedure Read(mem      : in out Stats_Type;
                  address  : in Address_Type) is
      cycles : Time_Type := 0;
   begin
      if mem.mem /= null then
         Start(mem.mem.all);
         Read(mem.mem.all, address);
         Commit(mem.mem.all, cycles);
         Advance(mem, cycles);
      end if;
      mem.reads := mem.reads + 1;
      Process(mem, address);
   end Read;

   procedure Write(mem     : in out Stats_Type;
                   address : in Address_Type) is
      cycles : Time_Type := 0;
   begin
      if mem.mem /= null then
         Start(mem.mem.all);
         Write(mem.mem.all, address);
         Commit(mem.mem.all, cycles);
         Advance(mem, cycles);
      end if;
      mem.writes := mem.writes + 1;
      Process(mem, address);
   end Write;

   procedure Show_Access_Stats(mem : in Stats_Type) is
   begin
      if mem.mem /= null then
         Show_Access_Stats(mem.mem.all);
      end if;
      Put_Line("    Reads:   " & Long_Integer'Image(mem.reads));
      Put_Line("    Writes:  " & Long_Integer'Image(mem.writes));
      Put_Line("    Accesses:" & Long_Integer'Image(mem.reads + mem.writes));
      mem.addresses.Show("    Addresses");
      mem.strides.Show("   Strides");
      mem.multipliers.Show("   Multipliers");
   end Show_Access_Stats;

   procedure Finalize(mem : in out Stats_Type) is
   begin
      if mem.mem /= null then
         Destroy(Memory_Pointer(mem.mem));
      end if;
   end Finalize;

end Memory.Stats;
