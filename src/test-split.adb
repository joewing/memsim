
with Memory.Split;   use Memory.Split;
with Memory.Join;    use Memory.Join;

package body Test.Split is

   procedure Run_Tests is

      ram   : constant Monitor_Pointer := new Monitor_Type;
      mon1  : constant Monitor_Pointer := new Monitor_Type;
      mon2  : constant Monitor_Pointer := new Monitor_Type;
      join1 : constant Join_Pointer    := Create_Join;
      join2 : constant Join_Pointer    := Create_Join;
      split : Split_Pointer;

   begin

      Set_Memory(mon1.all, join1);
      Set_Memory(mon2.all, join2);
      split := Create_Split(ram, mon1, mon2, 256);

      Check(ram.reads = 0);
      Check(ram.writes = 0);

      Read(split.all, 0, 8);
      Check(mon1.reads = 1);
      Check(mon2.reads = 0);
      Check(ram.reads = 1);

      Read(split.all, 256, 8);
      Check(mon1.reads = 1);
      Check(mon2.reads = 1);
      Check(ram.reads = 2);

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

      Read(split.all, Address_Type(0) - 4, 8);
      Check(mon1.reads = 3);
      Check(mon2.reads = 3);
      Check(ram.reads = 6);

      Read(split.all, Address_Type'Last, 1);
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

      Read(split.all, Address_Type'Last, 2);
      Check(mon1.reads = 6);
      Check(mon2.reads = 6);
      Check(ram.reads = 12);
      Check(Get_Time(ram.all) = Get_Time(split.all));

      Destroy(Memory_Pointer(split));

   end Run_Tests;

end Test.Split;
