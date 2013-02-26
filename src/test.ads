
package Test is

   procedure Run_Tests;

private

   procedure Check(cond : in Boolean;
                   msg  : in String);

   count    : Natural := 0;
   failed   : Natural := 0;

end Test;
