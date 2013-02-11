
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.Stats is

   Max_Stride : constant := Integer'Last;

   function Create_Stats(mem : access Memory_Type'Class)
                        return Stats_Pointer is
      result : constant Stats_Pointer := new Stats_Type;
   begin
      Set_Memory(result.all, mem);
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
                     address  : in Address_Type;
                     size     : in Positive) is
      stride : constant Integer := Compute_Stride(mem.last_address, address);
      mult   : constant Integer := Compute_Multiple(mem.last_stride, stride);
   begin
      mem.addresses.Increment(address);
      mem.strides.Increment(stride);
      mem.multipliers.Increment(mult);
      mem.last_address  := address;
      mem.last_stride   := stride;
      if address < mem.min_address then
         mem.min_address := address;
      end if;
      if address + Address_Type(size) > mem.max_address then
         mem.max_address := address + Address_Type(size);
      end if;
   end Process;

   procedure Read(mem      : in out Stats_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Read(Container_Type(mem), address, size);
      mem.reads := mem.reads + 1;
      Process(mem, address, size);
   end Read;

   procedure Write(mem     : in out Stats_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Write(Container_Type(mem), address, size);
      mem.writes := mem.writes + 1;
      Process(mem, address, size);
   end Write;

   procedure Show_Access_Stats(mem : in Stats_Type) is
   begin
      Show_Access_Stats(Container_Type(mem));
      Put_Line("    Reads:      " & Long_Integer'Image(mem.reads));
      Put_Line("    Writes:     " & Long_Integer'Image(mem.writes));
      Put_Line("    Min Address:" & Address_Type'Image(mem.min_address));
      Put_Line("    Max Address:" & Address_Type'Image(mem.max_address));
      mem.addresses.Show  ("    Addresses");
      mem.strides.Show    ("   Strides");
      mem.multipliers.Show("   Multipliers");
   end Show_Access_Stats;

end Memory.Stats;
