
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

      Check(ram.reads = 0, "split1");
      Check(ram.writes = 0, "split2");

      Read(split.all, 0, 8);
      Check(mon1.reads = 1, "split3");
      Check(mon2.reads = 0, "split4");
      Check(ram.reads = 1, "split5");

      Read(split.all, 256, 8);
      Check(mon1.reads = 1, "split6");
      Check(mon2.reads = 1, "split7");
      Check(ram.reads = 2, "split8");

      Write(split.all, 256, 8);
      Check(mon1.writes = 0, "split9");
      Check(mon2.writes = 1, "split10");
      Check(ram.writes = 1, "split11");

      Write(split.all, 0, 8);
      Check(mon1.writes = 1, "split12");
      Check(mon2.writes = 1, "split13");
      Check(ram.writes = 2, "split14");

      Idle(split.all, 10);
      Check(mon1.cycles = 10, "split15");
      Check(mon2.cycles = 10, "split16");
      Check(ram.cycles = 10, "split17");

      Read(split.all, 252, 8);
      Check(mon1.reads = 2, "split18");
      Check(mon2.reads = 2, "split19");
      Check(ram.reads = 4, "split20");

      Read(split.all, Address_Type(0) - 4, 8);
      Check(mon1.reads = 3, "split21");
      Check(mon2.reads = 3, "split22");
      Check(ram.reads = 6, "split23");

      Read(split.all, Address_Type'Last, 1);
      Check(mon1.reads = 3, "split24");
      Check(mon2.reads = 4, "split25");
      Check(ram.reads = 7, "split26");

      Read(split.all, 255, 1);
      Check(mon1.reads = 4, "split27");
      Check(mon2.reads = 4, "split28");
      Check(ram.reads = 8, "split29");

      Read(split.all, 255, 2);
      Check(mon1.reads = 5, "split30");
      Check(mon2.reads = 5, "split31");
      Check(ram.reads = 10, "split32");

      Read(split.all, Address_Type'Last, 2);
      Check(mon1.reads = 6, "split33");
      Check(mon2.reads = 6, "split34");
      Check(ram.reads = 12, "split35");
      Check(Get_Time(ram.all) = Get_Time(split.all), "split36");

      Destroy(Memory_Pointer(split));

   end Run_Tests;

end Test.Split;
