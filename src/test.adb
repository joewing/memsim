
with Ada.Text_IO; use Ada.Text_IO;

with Test.RAM;
with Test.Cache;
with Test.SPM;
with Test.Offset;
with Test.Shift;
with Test.Split;
with Test.Prefetch;

package body Test is

   function Clone(mem : Monitor_Type) return Memory_Pointer is
   begin
      return null;
   end Clone;

   procedure Read(mem      : in out Monitor_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Read(Container_Type(mem), address, size);
      mem.last_addr  := address;
      mem.last_size  := size;
      mem.reads      := mem.reads + 1;
   end Read;

   procedure Write(mem     : in out Monitor_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Write(Container_Type(mem), address, size);
      mem.last_addr  := address;
      mem.last_size  := size;
      mem.writes     := mem.writes + 1;
   end Write;

   procedure Idle(mem      : in out Monitor_Type;
                  cycles   : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
      mem.cycles := mem.cycles + cycles;
   end Idle;

   procedure Run_Tests is
   begin

      count    := 0;
      failed   := 0;

      RAM.Run_Tests;
      Cache.Run_Tests;
      SPM.Run_Tests;
      Offset.Run_Tests;
      Shift.Run_Tests;
      Split.Run_Tests;
      Prefetch.Run_Tests;

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
