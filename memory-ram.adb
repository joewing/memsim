
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.RAM is

   function Create_RAM(latency : Natural := 1) return RAM_Pointer is
      result : constant RAM_Pointer := new RAM_Type;
   begin
      result.latency := latency;
      return result;
   end Create_RAM;

   procedure Read(mem       : in out RAM_Type;
                  address   : Address_Type) is
   begin
      Put_Line("RAM Read: " & Address_Type'image(address));
      Advance(mem, mem.latency);
   end Read;

   procedure Write(mem      : in out RAM_Type;
                   address  : Address_Type) is
   begin
      Put_Line("RAM Write: " & Address_Type'image(address));
      Advance(mem, mem.latency);
   end Write;

end Memory.RAM;
