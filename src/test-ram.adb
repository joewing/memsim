
with Memory.RAM; use Memory.RAM;

package body Test.RAM is

   procedure Run_Tests is

      mem : RAM_Pointer := Create_RAM(latency      => 100,
                                      word_size    => 4,
                                      word_count   => 8);
      other : Memory_Pointer := null;

   begin

      Check(Get_Time(mem.all) = 0);
      Check(Get_Writes(mem.all) = 0);
      Check(Get_Cost(mem.all) = 0);

      Read(mem.all, 0, 4);
      Check(Get_Time(mem.all) = 100);
      Check(Get_Writes(mem.all) = 0);

      Read(mem.all, 2, 4);
      Check(Get_Time(mem.all) = 300);
      Check(Get_Writes(mem.all) = 0);

      Read(mem.all, 2, 2);
      Check(Get_Time(mem.all) = 400);
      Check(Get_Writes(mem.all) = 0);

      Read(mem.all, 2, 3);
      Check(Get_Time(mem.all) = 600);
      Check(Get_Writes(mem.all) = 0);

      Write(mem.all, 1, 8);
      Check(Get_Time(mem.all) = 900);
      Check(Get_Writes(mem.all) = 1);

      Reset(mem.all);
      Check(Get_Time(mem.all) = 0);

      Check(To_String(mem.all) =
            "(ram (latency 100)(word_size 4)(word_count 8))");

      other := Clone(mem.all);
      Check(To_String(other.all) = To_String(mem.all));

      Destroy(Memory_Pointer(mem));

   end Run_Tests;

end Test.RAM;
