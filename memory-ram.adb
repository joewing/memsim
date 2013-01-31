
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.RAM is

   function Create_RAM(clock     : Clock_Pointer;
                       latency   : Natural := 1) return RAM_Pointer is
      result : RAM_Pointer := new RAM_Type;
   begin
      result.clock := clock;
      result.latency := latency;
      return result;
   end Create_RAM;

   function Read(mem       : Memory_Pointer;
                 address   : Address_Type) return Natural is
   begin
      Put_Line("RAM Read: " & Address_Type'image(address));
      return mem.latency;
   end Read;

   function Write(mem      : RAM_Pointer;
                  address : Address_Type) return Natural is
   begin
      Put_Line("RAM Write: " & Address_Type'image(address));
      return mem.latency;
   end Write;

end Memory.RAM;
