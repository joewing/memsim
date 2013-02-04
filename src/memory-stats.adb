
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.Stats is

   function Create_Stats(mem : access Memory_Type'Class)
                        return Stats_Pointer is
      result : constant Stats_Pointer := new Stats_Type;
   begin
      result.mem := mem;
      return result;
   end Create_Stats;

   procedure Increment(m : in out Stride_Maps.Map;
                       i : in Integer) is
      value : Natural := 1;
   begin
      if Stride_Maps.Contains(m, i) then
         value := Stride_Maps.Element(m, i) + 1;
         Stride_Maps.Replace(m, i, value);
      else
         Stride_Maps.Insert(m, i, value);
      end if;
   end Increment;

   function Compute_Multiple(last, current : Integer) return Integer is
   begin
      if last /= 0 then
         return current / last;
      else
         return 0;
      end if;
   end Compute_Multiple;

   procedure Process(mem      : in out Stats_Type;
                     address  : in Address_Type) is
      iaddr  : constant Integer := Integer(address);
      stride : constant Integer := iaddr - mem.last_address;
      mult   : constant Integer := Compute_Multiple(mem.last_stride, stride);
   begin
      Increment(mem.addresses, iaddr);
      Increment(mem.strides, stride);
      Increment(mem.multipliers, mult);
      mem.last_address  := iaddr;
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

   procedure Show_Histogram(label   : in String;
                            m       : in Stride_Maps.Map) is
      procedure Show(pos : in Stride_Maps.Cursor) is
         key   : constant Integer := Stride_Maps.Key(pos);
         value : constant Natural := Stride_Maps.Element(pos);
      begin
         Put_Line("  " & Integer'Image(key) & ":" & Natural'Image(value));
      end Show;
   begin
      Put_Line(label & ":");
      Stride_Maps.Iterate(m, Show'Access);
   end Show_Histogram;

   procedure Show_Access_Stats(mem : in Stats_Type) is
   begin
      if mem.mem /= null then
         Show_Access_Stats(mem.mem.all);
      end if;
      Put_Line("Reads:" & Natural'Image(mem.reads));
      Put_Line("Writes:" & Natural'Image(mem.writes));
      Put_Line("Accesses:" & Natural'Image(mem.reads + mem.writes));
      Show_Histogram("Addresses", mem.addresses);
      Show_Histogram("Strides", mem.strides);
      Show_Histogram("Multipliers", mem.multipliers);
   end Show_Access_Stats;

end Memory.Stats;
