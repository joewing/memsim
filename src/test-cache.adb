
with Memory.RAM; use Memory.RAM;
with Memory.Cache; use Memory.Cache;

package body Test.Cache is

   procedure Test_Direct is

      ram1 : constant RAM_Pointer := Create_RAM(latency     => 100,
                                                word_size   => 1);
      cache1 : Cache_Pointer := Create_Cache(mem            => ram1,
                                             line_count     => 4,
                                             line_size      => 2,
                                             associativity  => 1,
                                             latency        => 1,
                                             policy         => LRU,
                                             exclusive      => False,
                                             write_back     => True);

   begin

      Check(Get_Time(cache1.all) = 0, "cache-direct1");
      Check(Get_Writes(cache1.all) = 0, "cache-direct2");

      Read(cache1.all, 0, 1);
      Check(Get_Time(cache1.all) = 200, "cache-direct3");

      Read(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 201, "cache-direct4");

      Write(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 202, "cache-direct5");
      Check(Get_Writes(ram1.all) = 0, "cache-direct6");

      Read(cache1.all, 8, 1);
      Check(Get_Time(cache1.all) = 602, "cache-direct7");
      Check(Get_Writes(ram1.all) = 1, "cache-direct8");

      Read(cache1.all, 2, 2);
      Check(Get_Time(cache1.all) = 802, "cache-direct9");

      Write(cache1.all, 4, 2);
      Check(Get_Time(cache1.all) = 803, "cache-direct10");

      Write(cache1.all, 6, 1);
      Check(Get_Time(cache1.all) = 1003, "cache-direct11");

      Destroy(Memory_Pointer(cache1));

   end Test_Direct;

   procedure Test_Set is

      ram1 : constant RAM_Pointer := Create_RAM(latency     => 100,
                                                word_size   => 1);
      cache1 : Cache_Pointer := Create_Cache(mem            => ram1,
                                             line_count     => 4,
                                             line_size      => 2,
                                             associativity  => 2,
                                             latency        => 1,
                                             policy         => LRU,
                                             exclusive      => False,
                                             write_back     => True);

   begin

      Read(cache1.all, 0, 1);
      Check(Get_Time(cache1.all) = 200, "cache-set1");

      Read(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 201, "cache-set2");

      Write(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 202, "cache-set3");
      Check(Get_Writes(ram1.all) = 0, "cache-set4");

      Read(cache1.all, 8, 1);
      Check(Get_Time(cache1.all) = 402, "cache-set5");
      Check(Get_Writes(ram1.all) = 0, "cache-set6");

      Read(cache1.all, 2, 2);
      Check(Get_Time(cache1.all) = 602, "cache-set7");

      Write(cache1.all, 4, 2);
      Check(Get_Time(cache1.all) = 803, "cache-set8");

      Write(cache1.all, 6, 1);
      Check(Get_Time(cache1.all) = 1003, "cache-set9");

      Destroy(Memory_Pointer(cache1));

   end Test_Set;

   procedure Run_Tests is
   begin

      Test_Direct;
      Test_Set;

   end Run_Tests;

end Test.Cache;
