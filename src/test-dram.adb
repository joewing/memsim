
with Memory.DRAM; use Memory.DRAM;

package body Test.DRAM is

   procedure Test_Open is

      mem : DRAM_Pointer := Create_DRAM(cas_cycles       => 2,
                                        rcd_cycles       => 3,
                                        rp_cycles        => 4,
                                        wb_cycles        => 1,
                                        multiplier       => 2,
                                        word_size        => 4,
                                        page_size        => 1024,
                                        page_count       => 2048,
                                        width            => 2,
                                        burst_size       => 2,
                                        open_page_mode   => True);

   begin

      Check(Get_Time(mem.all) = 0);
      Check(Get_Writes(mem.all) = 0);
      Check(Get_Cost(mem.all) = 0);

      Read(mem.all, 0, 4);    -- Miss: 2 * (2 * 2 + 3 + 4) = 22
      Check(Get_Time(mem.all) = 22);
      Check(Get_Writes(mem.all) = 0);

      Read(mem.all, 4, 4);    -- Hit: 2 * (2 * 2) = 8
      Check(Get_Time(mem.all) = 22 + 8);

      Read(mem.all, 2097152, 4); -- Miss (new bank): 2 * (2 * 2 + 3 + 4) = 22
      Check(Get_Time(mem.all) = 22 + 8 + 22);

      Read(mem.all, 8, 8);    -- Hit: 2 * (2 * 4) = 16
      Check(Get_Time(mem.all) = 22 + 8 + 22 + 16);

      Write(mem.all, 2097152, 8);   -- Hit: 2 * (2 * 4) = 16
      Check(Get_Time(mem.all) = 22 + 8 + 22 + 16 + 16);
      Check(Get_Writes(mem.all) = 1);

      Write(mem.all, 2097152 - 4, 8);  -- Miss/Hit
                                       -- 2 * (2 * 2 + 3 + 4) +
                                       -- 2 * (2 * 2) = 32
      Check(Get_Time(mem.all) = 22 + 8 + 22 + 16 + 16 + 30);
      Check(Get_Writes(mem.all) = 2);

      Read(mem.all, 4, 4);    -- Miss+wb: 2 * (2 * 2 + 3 + 4 + 1) = 24
      Check(Get_Time(mem.all) = 22 + 8 + 22 + 16 + 16 + 30 + 24);
      Check(Get_Writes(mem.all) = 2);

      Destroy(Memory_Pointer(mem));

   end Test_Open;

   procedure Test_Closed is

      mem : DRAM_Pointer := Create_DRAM(cas_cycles       => 2,
                                        rcd_cycles       => 3,
                                        rp_cycles        => 4,
                                        wb_cycles        => 1,
                                        multiplier       => 2,
                                        word_size        => 4,
                                        page_size        => 1024,
                                        page_count       => 16384,
                                        width            => 2,
                                        burst_size       => 2,
                                        open_page_mode   => False);

   begin

      Check(Get_Time(mem.all) = 0);
      Check(Get_Writes(mem.all) = 0);
      Check(Get_Cost(mem.all) = 0);

      Destroy(Memory_Pointer(mem));

   end Test_Closed;

   procedure Run_Tests is
   begin
      Test_Open;
      Test_Closed;
   end Run_Tests;

end Test.DRAM;
