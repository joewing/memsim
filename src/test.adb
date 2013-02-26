
with Ada.Text_IO; use Ada.Text_IO;
with Test.RAM;
with Test.Cache;

package body Test is

   procedure Run_Tests is
   begin

      count    := 0;
      failed   := 0;

      RAM.Run_Tests;
      Cache.Run_Tests;

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
