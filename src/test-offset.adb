
with Memory.RAM;              use Memory.RAM;
with Memory.Transform.Offset; use Memory.Transform.Offset;

package body Test.Offset is

   procedure Run_Tests is

      ram      : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                     word_size => 8);
      offset   : Offset_Pointer := Create_Offset;

   begin

      Set_Memory(offset.all, ram);
      Set_Offset(offset.all, 3);

      Check(Get_Time(ram.all) = 0);
      Check(Get_Writes(ram.all) = 0);
      Check(Get_Cost(offset.all) = 0);

      Read(offset.all, 0, 8);
      Check(Get_Time(ram.all) = 200);
      Check(Get_Writes(ram.all) = 0);

      Read(offset.all, 5, 8);
      Check(Get_Time(ram.all) = 300);
      Check(Get_Writes(ram.all) = 0);

      Write(offset.all, 5, 4);
      Check(Get_Time(ram.all) = 400);
      Check(Get_Writes(ram.all) = 1);

      Write(offset.all, 2, 8);
      Check(Get_Time(ram.all) = 600);
      Check(Get_Writes(ram.all) = 2);

      Read(offset.all, Address_Type(0) - 6, 8);
      Check(Get_Time(ram.all) = 800);

      Destroy(Memory_Pointer(offset));

   end Run_Tests;

end Test.Offset;
