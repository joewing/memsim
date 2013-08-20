
with Memory.Register;         use Memory.Register;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Join;             use Memory.Join;

package body Test.Register is

   procedure Test_Insert is

      ram      : constant Monitor_Pointer := Create_Monitor(10);
      offset   : Offset_Pointer           := Create_Offset;
      bank     : constant Offset_Pointer  := Create_Offset;
      join     : constant Join_Pointer    := Create_Join(offset, 0);

   begin

      Set_Value(offset.all, 4);
      Set_Value(bank.all, 4);
      Set_Memory(offset.all, ram);
      Set_Memory(bank.all, join);
      Set_Bank(offset.all, bank);

      Check(Get_Path_Length(ram.all) = 0);
      Check(Get_Path_Length(bank.all) = 64);
      Check(Get_Path_Length(offset.all) = 96);
      Check(Get_Time(offset.all) = 0);
      Check(Get_Writes(offset.all) = 0);
      Check(Get_Cost(offset.all) = 0);

      Read(offset.all, 0, 1);
      Check(ram.reads = 1);
      Check(Get_Time(offset.all) = 11);
      Check(Get_Time(ram.all) = 11);

      Write(offset.all, 0, 1);
      Check(Get_Time(offset.all) = 22);
      Check(Get_Writes(offset.all) = 1);

      Insert_Registers(offset);
      Check(Get_Path_Length(ram.all) = 0);
      Check(Get_Path_Length(bank.all) = 64);
      Check(Get_Path_Length(offset.all) = 32);
      Check(Get_Max_Length(offset) = 64);

      Reset(offset.all, 0);
      Check(Get_Time(offset.all) = 0);

      Read(offset.all, 0, 1);
      Check(Get_Time(offset.all) = 12);
      Check(Get_Time(ram.all) = 11);

      Write(offset.all, 0, 1);
      Check(Get_Time(offset.all) = 24);
      Check(Get_Writes(offset.all) = 1);

      offset := Offset_Pointer(Remove_Registers(Memory_Pointer(offset)));
      Reset(offset.all, 0);
      Check(Get_Time(offset.all) = 0);

      Read(offset.all, 0, 1);
      Check(Get_Time(offset.all) = 11);
      Check(Get_Time(ram.all) = 11);

      Write(offset.all, 0, 1);
      Check(Get_Time(offset.all) = 22);
      Check(Get_Writes(offset.all) = 1);

      Destroy(Memory_Pointer(offset));

   end Test_Insert;

   procedure Run_Tests is
   begin
      Test_Insert;
   end Run_Tests;

end Test.Register;
