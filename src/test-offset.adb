
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Join;             use Memory.Join;

package body Test.Offset is

   procedure Run_Tests is

      mem      : constant Monitor_Pointer := new Monitor_Type;
      bank     : constant Monitor_Pointer := new Monitor_Type;
      offset   : Offset_Pointer           := Create_Offset;
      join     : constant Join_Pointer    := Create_Join(offset, 0);

   begin

      Set_Memory(bank.all, join);
      Set_Memory(offset.all, mem);
      Set_Bank(offset.all, bank);
      Set_Offset(offset.all, 3);

      Check(Get_Time(mem.all) = 0);
      Check(Get_Time(offset.all) = 0);
      Check(Get_Writes(offset.all) = 0);
      Check(Get_Cost(offset.all) = 0);

      Read(offset.all, 0, 8);

      Read(offset.all, 5, 8);

      Write(offset.all, 5, 4);

      Write(offset.all, 2, 8);

      Read(offset.all, Address_Type(0) - 6, 8);

      Destroy(Memory_Pointer(offset));

   end Run_Tests;

end Test.Offset;
