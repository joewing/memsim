
with Memory.RAM;  use Memory.RAM;
with Memory.SPM;  use Memory.SPM;

package body Test.SPM is

   procedure Run_Tests is

      ram   : constant RAM_Pointer  := Create_RAM(latency   => 100,
                                                  word_size => 8);
      spm   : SPM_Pointer           := Create_SPM(ram, 1024, 1);

   begin

      Check(Get_Time(spm.all) = 0);
      Check(Get_Writes(spm.all) = 0);
      Check(Get_Cost(spm.all) = 1);

      Read(spm.all, 0, 1);
      Check(Get_Time(spm.all) = 1);
      Check(Get_Writes(spm.all) = 0);

      Read(spm.all, 1024 - 8, 8);
      Check(Get_Time(spm.all) = 2);
      Check(Get_Writes(spm.all) = 0);

      Read(spm.all, 1024, 4);
      Check(Get_Time(spm.all) = 102);
      Check(Get_Writes(spm.all) = 0);

      Read(spm.all, 1023, 2);
      Check(Get_Time(spm.all) = 202);
      Check(Get_Writes(spm.all) = 0);

      Write(spm.all, 1024, 1);
      Check(Get_Time(spm.all) = 302);
      Check(Get_Writes(spm.all) = 1);

      Write(spm.all, 8, 1);
      Check(Get_Time(spm.all) = 303);
      Check(Get_Writes(spm.all) = 1);

      Write(spm.all, 8192, 16);
      Check(Get_Time(spm.all) = 503);
      Check(Get_Writes(spm.all) = 2);

      Destroy(Memory_Pointer(spm));

   end Run_Tests;

end Test.SPM;
