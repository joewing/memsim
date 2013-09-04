
with Ada.Text_IO; use Ada.Text_IO;

with Memory.RAM; use Memory.RAM;
with Device;
with Test.Cache;
with Test.DRAM;
with Test.Flip;
with Test.Offset;
with Test.Prefetch;
with Test.RAM;
with Test.Register;
with Test.Shift;
with Test.Split;
with Test.SPM;

package body Test is

   function Create_Monitor(latency  : Time_Type := 0;
                           ram      : Boolean   := True)
                           return Monitor_Pointer is
      result : constant Monitor_Pointer := new Monitor_Type;
   begin
      result.latency := latency;
      if ram then
         Set_Memory(result.all, Create_RAM(latency => 1, word_size => 8));
      end if;
      return result;
   end Create_Monitor;

   function Clone(mem : Monitor_Type) return Memory_Pointer is
   begin
      return null;
   end Clone;

   procedure Read(mem      : in out Monitor_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Check(address < Address_Type(2) ** 32);
      Read(Container_Type(mem), address, size);
      mem.last_addr  := address;
      mem.last_size  := size;
      mem.reads      := mem.reads + 1;
      Advance(mem, mem.latency);
      mem.cycles     := mem.cycles + mem.latency;
   end Read;

   procedure Write(mem     : in out Monitor_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Check(address < Address_Type(2) ** 32);
      Write(Container_Type(mem), address, size);
      mem.last_addr  := address;
      mem.last_size  := size;
      mem.writes     := mem.writes + 1;
      Advance(mem, mem.latency);
      mem.cycles     := mem.cycles + mem.latency;
   end Write;

   procedure Idle(mem      : in out Monitor_Type;
                  cycles   : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
      mem.cycles := mem.cycles + cycles;
   end Idle;

   procedure Generate(mem  : in Monitor_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      other : constant Memory_Pointer := Get_Memory(mem);
   begin
      Generate(other.all, sigs, code);
   end Generate;

   procedure Run_Tests is
   begin

      count    := 0;
      failed   := 0;

      Device.Set_Device("virtex7");
      Device.Set_Address_Bits(32);

      RAM.Run_Tests;
      Cache.Run_Tests;
      DRAM.Run_Tests;
      SPM.Run_Tests;
      Flip.Run_Tests;
      Offset.Run_Tests;
      Shift.Run_Tests;
      Split.Run_Tests;
      Prefetch.Run_Tests;
      Register.Run_Tests;

      Put_Line("ran" & Natural'Image(count) & " tests");
      if failed > 1 then
         Put_Line(Natural'Image(failed) & " tests failed");
      elsif failed = 1 then
         Put_Line(Natural'Image(failed) & " test failed");
      else
         Put_Line("all tests passed");
      end if;

   end Run_Tests;

   procedure Check(cond    : in Boolean;
                   source  : in String := GNAT.Source_Info.File;
                   line    : in Natural := GNAT.Source_Info.Line) is
      lstr : constant String := Natural'Image(line);
      msg : constant String := source & "[" &
                               lstr(lstr'First + 1 .. lstr'Last) & "]";
   begin
      count := count + 1;
      if not cond then
         Put_Line(msg & ": FAILED");
         failed := failed + 1;
      end if;
   end Check;

end Test;
