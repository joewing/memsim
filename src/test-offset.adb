
with Memory.RAM;              use Memory.RAM;
with Memory.Transform.Offset; use Memory.Transform.Offset;

package body Test.Offset is

   procedure Run_Tests is

      ram      : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                     word_size => 8);
      offset   : Offset_Pointer := Create_Offset(ram, 3);

   begin

      Check(Get_Time(ram.all) = 0, "offset1");
      Check(Get_Writes(ram.all) = 0, "offset2");

      Read(offset.all, 0, 8);
      Check(Get_Time(ram.all) = 200, "offset3");
      Check(Get_Writes(ram.all) = 0, "offset4");

      Read(offset.all, 5, 8);
      Check(Get_Time(ram.all) = 300, "offset5");
      Check(Get_Writes(ram.all) = 0, "offset6");

      Write(offset.all, 5, 4);
      Check(Get_Time(ram.all) = 400, "offset7");
      Check(Get_Writes(ram.all) = 1, "offset8");

      Write(offset.all, 2, 8);
      Check(Get_Time(ram.all) = 600, "offset9");
      Check(Get_Writes(ram.all) = 2, "offset10");

      Read(offset.all, Address_Type(0) - 6, 8);
      Check(Get_Time(ram.all) = 800, "offset11");

      Destroy(Memory_Pointer(offset));

   end Run_Tests;

end Test.Offset;
