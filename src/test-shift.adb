
with Memory.RAM;              use Memory.RAM;
with Memory.Transform.Shift;  use Memory.Transform.Shift;
use Memory;

with Ada.Text_IO; use Ada.Text_IO;

package body Test.Shift is

   procedure Test_Positive is

      ram   : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                  word_size => 8);
      shift : Shift_Pointer         := Create_Shift(ram, 2);

   begin

      Check(Get_Time(shift.all) = 0, "shift.positive1");
      Check(Get_Writes(shift.all) = 0, "shift.positive2");

      Read(shift.all, 0, 8);
      Check(Get_Time(shift.all) = 800, "shift.positive3");
      Check(Get_Writes(shift.all) = 0, "shift.positive4");

      Read(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 900, "shift.positive5");
      Check(Get_Writes(shift.all) = 0, "shift.positive6");

      Write(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 1000, "shift.positive7");
      Check(Get_Writes(shift.all) = 1, "shift.positive8");

      Destroy(Memory_Pointer(shift));

   end Test_Positive;

   procedure Test_Negative is

      ram   : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                  word_size => 8);
      shift : Shift_Pointer         := Create_Shift(ram, -1);

   begin

      Check(Get_Time(shift.all) = 0, "shift.negative1");
      Check(Get_Writes(shift.all) = 0, "shift.negative2");

      Read(shift.all, 0, 8);
      Check(Get_Time(shift.all) = 800, "shift.negative3");
      Check(Get_Writes(shift.all) = 0, "shift.negative4");

      Read(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 900, "shift.negative5");
      Check(Get_Writes(shift.all) = 0, "shift.negative6");

      Write(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 1000, "shift.negative7");
      Check(Get_Writes(shift.all) = 1, "shift.negative8");

      Destroy(Memory_Pointer(shift));

   end Test_Negative;

   procedure Run_Tests is
   begin
      Test_Positive;
      Test_Negative;
   end Run_Tests;

end Test.Shift;
