
with Memory.RAM;              use Memory.RAM;
with Memory.Transform.Shift;  use Memory.Transform.Shift;

package body Test.Shift is

   procedure Test_Positive is

      ram   : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                  word_size => 8);
      shift : Shift_Pointer         := Create_Shift(ram, 1, 2);

   begin

      Check(Get_Time(shift.all) = 0);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 0, 8);
      Check(Get_Time(shift.all) = 800);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 900);
      Check(Get_Writes(shift.all) = 0);

      Write(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 1000);
      Check(Get_Writes(shift.all) = 1);

      Destroy(Memory_Pointer(shift));

   end Test_Positive;

   procedure Test_Negative is

      ram   : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                  word_size => 8);
      shift : Shift_Pointer         := Create_Shift(ram, 1, -1);

   begin

      Check(Get_Time(shift.all) = 0);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 0, 8);
      Check(Get_Time(shift.all) = 800);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 900);
      Check(Get_Writes(shift.all) = 0);

      Write(shift.all, 1, 1);
      Check(Get_Time(shift.all) = 1000);
      Check(Get_Writes(shift.all) = 1);

      Destroy(Memory_Pointer(shift));

   end Test_Negative;

   procedure Test_Word_Size is

      ram   : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                  word_size => 8);
      shift : Shift_Pointer         := Create_Shift(ram, 4, 1);

   begin

      Check(Get_Time(shift.all) = 0);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 0, 8);
      Check(Get_Time(shift.all) = 200);
      Check(Get_Writes(shift.all) = 0);

      Write(shift.all, 1, 4);
      Check(Get_Time(shift.all) = 400);
      Check(Get_Writes(shift.all) = 2);

      Destroy(Memory_Pointer(shift));

   end Test_Word_Size;

   procedure Run_Tests is
   begin
      Test_Positive;
      Test_Negative;
      Test_Word_Size;
   end Run_Tests;

end Test.Shift;
