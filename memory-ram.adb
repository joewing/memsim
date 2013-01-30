
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.RAM is

   procedure Read(mem : access RAM; address : Address_Type) is
   begin
      Put_Line("Time: " & Natural'image(mem.time)
               & " Read: " & Address_Type'image(address));
      mem.time := mem.time + mem.latency;
   end Read;

   procedure Write(mem : access RAM; address : Address_Type) is
   begin
      Put_Line("Time: " & Natural'image(mem.time)
               & " Write: " & Address_Type'image(address));
      mem.time := mem.time + mem.latency;
   end Write;

   procedure Set_Latency(mem : access RAM; latency : Natural) is
   begin
      mem.latency := latency;
   end Set_Latency;

end Memory.RAM;
