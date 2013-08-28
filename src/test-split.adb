
with Memory.Split;   use Memory.Split;
with Memory.Join;    use Memory.Join;

package body Test.Split is

   procedure Run_Tests is

      ram   : constant Monitor_Pointer := Create_Monitor(0);
      mon1  : constant Monitor_Pointer := Create_Monitor(0, False);
      mon2  : constant Monitor_Pointer := Create_Monitor(0, False);
      split : Split_Pointer            := Create_Split;
      join1 : constant Join_Pointer    := Create_Join(split, 0);
      join2 : constant Join_Pointer    := Create_Join(split, 1);

   begin

      Set_Offset(split.all, 256);
      Set_Memory(split.all, ram);
      Set_Bank(split.all, 0, mon1);
      Set_Bank(split.all, 1, mon2);
      Set_Memory(mon1.all, join1);
      Set_Memory(mon2.all, join2);

      Check(ram.reads = 0);
      Check(ram.writes = 0);
      Check(Get_Cost(split.all) = Get_Cost(mon1.all) + Get_Cost(mon2.all));

      Read(split.all, 0, 8);
      Check(mon1.reads = 1);
      Check(mon1.last_addr = 0);
      Check(mon1.last_size = 8);
      Check(mon2.reads = 0);
      Check(ram.reads = 1);
      Check(ram.last_addr = 0);
      Check(ram.last_size = 8);

      Read(split.all, 256, 8);
      Check(mon1.reads = 1);
      Check(mon1.last_addr = 0);
      Check(mon1.last_size = 8);
      Check(mon2.reads = 1);
      Check(mon2.last_addr = 0);
      Check(mon2.last_size = 8);
      Check(ram.reads = 2);
      Check(ram.last_addr = 256);
      Check(ram.last_size = 8);

      Write(split.all, 256, 8);
      Check(mon1.writes = 0);
      Check(mon2.writes = 1);
      Check(ram.writes = 1);

      Write(split.all, 0, 8);
      Check(mon1.writes = 1);
      Check(mon2.writes = 1);
      Check(ram.writes = 2);

      Idle(split.all, 10);
      Check(mon1.cycles = 10);
      Check(mon2.cycles = 10);
      Check(ram.cycles = 10);

      Read(split.all, 252, 8);
      Check(mon1.reads = 2);
      Check(mon2.reads = 2);
      Check(ram.reads = 4);

      Read(split.all, Address_Type(2) ** 32 - 4, 8);
      Check(mon1.reads = 3);
      Check(mon2.reads = 3);
      Check(ram.reads = 6);

      Read(split.all, Address_Type(2) ** 32 - 1, 1);
      Check(mon1.reads = 3);
      Check(mon2.reads = 4);
      Check(ram.reads = 7);

      Read(split.all, 255, 1);
      Check(mon1.reads = 4);
      Check(mon2.reads = 4);
      Check(ram.reads = 8);

      Read(split.all, 255, 2);
      Check(mon1.reads = 5);
      Check(mon2.reads = 5);
      Check(ram.reads = 10);

      Read(split.all, Address_Type(2) ** 32 - 1, 2);
      Check(mon1.reads = 6);
      Check(mon2.reads = 6);
      Check(ram.reads = 12);
      Check(Get_Time(ram.all) = Get_Time(split.all));

      Destroy(Memory_Pointer(split));

   end Run_Tests;

end Test.Split;
