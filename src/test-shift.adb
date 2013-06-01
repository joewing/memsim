
with Memory.Transform.Shift;  use Memory.Transform.Shift;
with Memory.Join;             use Memory.Join;

package body Test.Shift is

   procedure Test_Positive is

      mem   : constant Monitor_Pointer := new Monitor_Type;
      bank  : constant Monitor_Pointer := new Monitor_Type;
      shift : Shift_Pointer            := Create_Shift;
      join  : constant Join_Pointer    := Create_Join(shift, 0);

   begin

      Set_Memory(bank.all, join);
      Set_Bank(shift.all, bank);
      Set_Memory(shift.all, mem);
      Set_Shift(shift.all, 1);

      Check(Get_Time(shift.all) = 0);
      Check(Get_Writes(shift.all) = 0);
      Check(Get_Cost(shift.all) = 0);

      Read(shift.all, 0, 8);
      Check(mem.last_addr = 0);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 0);
      Check(bank.last_size = 8);
      Check(Get_Time(shift.all) = 1);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 1, 1);
      Check(mem.last_addr = 1);
      Check(mem.last_size = 1);
      Check(bank.last_addr = 1);
      Check(bank.last_size = 1);
      Check(Get_Time(shift.all) = 2);
      Check(Get_Writes(shift.all) = 0);

      Write(shift.all, 1, 1);
      Check(mem.last_addr = 1);
      Check(mem.last_size = 1);
      Check(bank.last_addr = 1);
      Check(bank.last_size = 1);
      Check(Get_Time(shift.all) = 3);
      Check(Get_Writes(shift.all) = 1);

      Read(shift.all, 16, 8);
      Check(mem.last_addr = 16);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 32);
      Check(bank.last_size = 8);
      Check(Get_Time(shift.all)  = 4);

      Read(shift.all, 2 ** 63, 4);
      Check(mem.last_addr = 2 ** 63);
      Check(mem.last_size = 4);
      Check(bank.last_addr = 8);
      Check(bank.last_size = 4);
      Check(Get_Time(shift.all) = 5);

      Read(shift.all, 105, 2);
      Check(mem.last_addr = 105);
      Check(mem.last_size = 2);
      Check(bank.last_addr = 209);
      Check(bank.last_size = 2);
      Check(Get_Time(shift.all) = 6);

      Destroy(Memory_Pointer(shift));

   end Test_Positive;

   procedure Test_Negative is

      mem   : constant Monitor_Pointer := new Monitor_Type;
      bank  : constant Monitor_Pointer := new Monitor_Type;
      shift : Shift_Pointer            := Create_Shift;
      join  : constant Join_Pointer    := Create_Join(shift, 0);

   begin

      Set_Memory(bank.all, join);
      Set_Bank(shift.all, bank);
      Set_Memory(shift.all, mem);
      Set_Shift(shift.all, -2);

      Check(Get_Time(shift.all) = 0);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 0, 8);
      Check(mem.last_addr = 0);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 0);
      Check(bank.last_size = 8);
      Check(Get_Time(shift.all) = 1);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 1, 1);
      Check(mem.last_addr = 1);
      Check(mem.last_size = 1);
      Check(bank.last_addr = 1);
      Check(bank.last_size = 1);
      Check(Get_Time(shift.all) = 2);
      Check(Get_Writes(shift.all) = 0);

      Write(shift.all, 1, 1);
      Check(mem.last_addr = 1);
      Check(mem.last_size = 1);
      Check(bank.last_addr = 1);
      Check(bank.last_size = 1);
      Check(Get_Time(shift.all) = 3);
      Check(Get_Writes(shift.all) = 1);

      Write(shift.all, 9, 4);
      Check(mem.last_addr = 9);
      Check(mem.last_size = 4);
      Check(bank.last_addr = ((Address_Type(2) ** 62) or 1));
      Check(bank.last_size = 4);
      Check(Get_Time(shift.all) = 4);
      Check(Get_Writes(shift.all) = 2);

      Destroy(Memory_Pointer(shift));

   end Test_Negative;

   procedure Run_Tests is
   begin
      Test_Positive;
      Test_Negative;
   end Run_Tests;

end Test.Shift;
