
with Memory.Transform.Flip;   use Memory.Transform.Flip;
with Memory.Join;             use Memory.Join;

package body Test.Flip is

   procedure Run_Tests is
      mem   : constant Monitor_Pointer := Create_Monitor(0);
      bank  : constant Monitor_Pointer := Create_Monitor(0, False);
      flip  : Flip_Pointer             := Create_Flip;
      join  : constant Join_Pointer    := Create_Join(flip, 0);
   begin

      Set_Memory(bank.all, join);
      Set_Bank(flip.all, bank);
      Set_Memory(flip.all, mem);

      Read(flip.all, 0, 8);
      Check(mem.last_addr = 0);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 0);
      Check(bank.last_size = 8);
      Check(Get_Time(flip.all) = 1);
      Check(Get_Writes(flip.all) = 0);

      Read(flip.all, 8, 8);
      Check(mem.last_addr = 8);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 2 ** 31);
      Check(bank.last_size = 8);
      Check(Get_Time(flip.all) = 2);
      Check(Get_Writes(flip.all) = 0);

      Write(flip.all, 2 ** 31, 8);
      Check(mem.last_addr = 2 ** 31);
      Check(mem.last_size = 8);
      Check(bank.last_addr = 8);
      Check(bank.last_size = 8);
      Check(Get_Time(flip.all) = 3);
      Check(Get_Writes(flip.all) = 1);

      Read(flip.all, 9, 4);
      Check(mem.last_addr = 9);
      Check(mem.last_size = 4);
      Check(bank.last_addr = ((2 ** 31) or 1));
      Check(bank.last_size = 4);
      Check(Get_Time(flip.all) = 4);
      Check(Get_Writes(flip.all) = 1);

      Destroy(Memory_Pointer(flip));

   end Run_Tests;

end Test.Flip;
