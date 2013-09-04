
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Join;             use Memory.Join;

package body Test.Offset is

   procedure Run_Tests is

      mem      : constant Monitor_Pointer := Create_Monitor(0, True);
      bank     : constant Monitor_Pointer := Create_Monitor(0, False);
      offset   : Offset_Pointer           := Create_Offset;
      join     : constant Join_Pointer    := Create_Join(offset, 0);

   begin

      Set_Memory(bank.all, join);
      Set_Memory(offset.all, mem);
      Set_Bank(offset.all, bank);
      Set_Value(offset.all, 3);

      Check(Get_Time(mem.all) = 0);
      Check(Get_Time(offset.all) = 0);
      Check(Get_Writes(offset.all) = 0);
      Check(Get_Cost(offset.all) = 0);

      Read(offset.all, 0, 8);
      Check(bank.last_addr = 3);
      Check(mem.last_addr = 0);
      Check(bank.reads = 1);
      Check(mem.reads = 1);
      Check(bank.writes = 0);
      Check(mem.writes = 0);

      Read(offset.all, 5, 8);
      Check(bank.last_addr = 8);
      Check(mem.last_addr = 5);
      Check(bank.reads = 2);
      Check(mem.reads = 2);
      Check(bank.writes = 0);
      Check(mem.writes = 0);

      Write(offset.all, 5, 4);
      Check(bank.last_addr = 8);
      Check(mem.last_addr = 5);
      Check(bank.reads = 2);
      Check(mem.reads = 2);
      Check(bank.writes = 1);
      Check(mem.writes = 1);

      Write(offset.all, 2, 8);
      Check(bank.last_addr = 5);
      Check(mem.last_addr = 2);
      Check(bank.reads = 2);
      Check(mem.reads = 2);
      Check(bank.writes = 2);
      Check(mem.writes = 2);

      Read(offset.all, Address_Type(2) ** 32 - 6, 8);
      Check(bank.last_addr = Address_Type(2) ** 32 - 3);
      Check(mem.last_addr = Address_Type(2) ** 32 - 6);
      Check(bank.reads = 3);
      Check(mem.reads = 3);
      Check(bank.writes = 2);
      Check(mem.writes = 2);

      Destroy(Memory_Pointer(offset));

   end Run_Tests;

end Test.Offset;
