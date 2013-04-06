
with Ada.Text_IO; use Ada.Text_IO;
with Test.RAM;
with Test.Cache;
with Test.SPM;
with Test.Offset;
with Test.Shift;
with Test.Split;

package body Test is

   function Clone(mem : Monitor_Type) return Memory_Pointer is
   begin
      return null;
   end Clone;

   procedure Read(mem      : in out Monitor_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      mem.reads := mem.reads + 1;
   end Read;

   procedure Write(mem     : in out Monitor_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      mem.writes := mem.writes + 1;
   end Write;

   procedure Idle(mem      : in out Monitor_Type;
                  cycles   : in Time_Type) is
   begin
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

      Put_Line("ran" & Natural'Image(count) & " tests");
      if failed > 1 then
         Put_Line(Natural'Image(failed) & " tests failed");
      elsif failed = 1 then
         Put_Line(Natural'Image(failed) & " test failed");
      else
         Put_Line("all tests passed");
      end if;

   end Run_Tests;

   procedure Check(cond : in Boolean;
                   msg  : in String) is
   begin
      count := count + 1;
      if cond then
         Put_Line(msg & " passed");
      else
         Put_Line(msg & " FAILED");
         failed := failed + 1;
      end if;
   end Check;

end Test;
