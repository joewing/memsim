
with Memory.Transform.Shift;  use Memory.Transform.Shift;
with Memory.Join;             use Memory.Join;

package body Test.Shift is

   procedure Test_Positive is

      mem   : constant Monitor_Pointer := Create_Monitor(0);
      bank  : constant Monitor_Pointer := Create_Monitor(1, False);
      shift : Shift_Pointer            := Create_Shift;
      join  : constant Join_Pointer    := Create_Join(shift, 0);

   begin

      Set_Memory(bank.all, join);
      Set_Bank(shift.all, bank);
      Set_Memory(shift.all, mem);
      Set_Value(shift.all, 1);

      Check(Get_Time(shift.all) = 0);
      Check(Get_Writes(shift.all) = 0);
      Check(Get_Cost(shift.all) = 0);

      Read(shift.all, 0, 8);
      Check(mem.last_addr = 0);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 0);
      Check(bank.last_size = 8);
      Check(Get_Time(shift.all) = 2);
      Check(Get_Writes(shift.all) = 0);

      Read(shift.all, 1, 1);
      Check(mem.last_addr = 1);
      Check(mem.last_size = 1);
      Check(bank.last_addr = 1);
      Check(bank.last_size = 1);
      Check(Get_Time(shift.all) = 4);
      Check(Get_Writes(shift.all) = 0);

      Write(shift.all, 1, 1);
      Check(mem.last_addr = 1);
      Check(mem.last_size = 1);
      Check(bank.last_addr = 1);
      Check(bank.last_size = 1);
      Check(Get_Time(shift.all) = 6);
      Check(Get_Writes(shift.all) = 1);

      Read(shift.all, 16, 8);
      Check(mem.last_addr = 16);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 32);
      Check(bank.last_size = 8);
      Check(Get_Time(shift.all)  = 8);

      Read(shift.all, 2 ** 31, 4);
      Check(mem.last_addr = 2 ** 31);
      Check(mem.last_size = 4);
      Check(bank.last_addr = 8);
      Check(bank.last_size = 4);
      Check(Get_Time(shift.all) = 10);

      Read(shift.all, 105, 2);
      Check(mem.last_addr = 105);
      Check(mem.last_size = 2);
      Check(bank.last_addr = 209);
      Check(bank.last_size = 2);
      Check(Get_Time(shift.all) = 12);

      Idle(shift.all, 4);
      Check(Get_Time(shift.all) = 16);

      Read(shift.all, 0, 1);
      Check(Get_Time(shift.all) = 18);

      Destroy(Memory_Pointer(shift));

   end Test_Positive;

   procedure Test_Negative is

      mem   : constant Monitor_Pointer := Create_Monitor(0, True);
      bank  : constant Monitor_Pointer := Create_Monitor(0, False);
      shift : Shift_Pointer            := Create_Shift;
      join  : constant Join_Pointer    := Create_Join(shift, 0);

   begin

      Set_Memory(bank.all, join);
      Set_Bank(shift.all, bank);
      Set_Memory(shift.all, mem);
      Set_Value(shift.all, -2);

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
      Check(bank.last_addr = ((Address_Type(2) ** 30) or 1));
      Check(bank.last_size = 4);
      Check(Get_Time(shift.all) = 4);
      Check(Get_Writes(shift.all) = 2);

      Destroy(Memory_Pointer(shift));

   end Test_Negative;

   procedure Test_Zero is

      mem   : constant Monitor_Pointer := Create_Monitor(0, True);
      bank  : constant Monitor_Pointer := Create_Monitor(0, False);
      shift : Shift_Pointer            := Create_Shift;
      join  : constant Join_Pointer    := Create_Join(shift, 0);

   begin

      Set_Memory(bank.all, join);
      Set_Bank(shift.all, bank);
      Set_Memory(shift.all, mem);
      Set_Value(shift.all, 0);

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
      Check(bank.last_addr = 9);
      Check(bank.last_size = 4);
      Check(Get_Time(shift.all) = 4);
      Check(Get_Writes(shift.all) = 2);

      Read(shift.all, 100, 16);
      Check(mem.last_addr = 100);
      Check(mem.last_size = 16);
      Check(bank.last_addr = 100);
      Check(bank.last_size = 16);
      Check(Get_Time(shift.all) = 7);
      Check(Get_Writes(shift.all) = 2);

      Read(shift.all, 16, 4);
      Check(mem.last_addr = 16);
      Check(mem.last_size = 4);
      Check(bank.last_addr = 16);
      Check(bank.last_size = 4);
      Check(Get_Time(shift.all) = 8);
      Check(Get_Writes(shift.all) = 2);

      Destroy(Memory_Pointer(shift));

   end Test_Zero;

   procedure Run_Tests is
   begin
      Test_Positive;
      Test_Negative;
      Test_Zero;
   end Run_Tests;

end Test.Shift;
