
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
                                             write_back     => True);

   begin

      Check(Get_Time(cache1.all) = 0);
      Check(Get_Writes(cache1.all) = 0);
      Check(Get_Cost(cache1.all) = 1);

      Read(cache1.all, 0, 1);
      Check(Get_Time(cache1.all) = 201);

      Read(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 202);

      Write(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 203);
      Check(Get_Writes(ram1.all) = 0);

      Read(cache1.all, 8, 1);
      Check(Get_Time(cache1.all) = 604);
      Check(Get_Writes(ram1.all) = 1);

      Read(cache1.all, 2, 2);
      Check(Get_Time(cache1.all) = 805);

      Write(cache1.all, 4, 2);
      Check(Get_Time(cache1.all) = 806);

      Write(cache1.all, 6, 1);
      Check(Get_Time(cache1.all) = 1007);

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
                                             write_back     => True);

   begin

      Check(Get_Cost(cache1.all) = 2);

      Read(cache1.all, 0, 1);
      Check(Get_Time(cache1.all) = 201);

      Read(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 202);

      Write(cache1.all, 1, 1);
      Check(Get_Time(cache1.all) = 203);
      Check(Get_Writes(ram1.all) = 0);

      Read(cache1.all, 8, 1);
      Check(Get_Time(cache1.all) = 404);
      Check(Get_Writes(ram1.all) = 0);

      Read(cache1.all, 2, 2);
      Check(Get_Time(cache1.all) = 605);

      Write(cache1.all, 4, 2);
      Check(Get_Time(cache1.all) = 806);

      Write(cache1.all, 6, 1);
      Check(Get_Time(cache1.all) = 1007);

      Destroy(Memory_Pointer(cache1));

   end Test_Set;

   procedure Test_Dual is

      ram1   : constant RAM_Pointer   := Create_RAM(latency     => 100,
                                                    word_size   => 1);
      cache1 : constant Cache_Pointer := Create_Cache(mem            => ram1,
                                                      line_count     => 2,
                                                      line_size      => 1,
                                                      associativity  => 1,
                                                      latency        => 1,
                                                      policy         => LRU,
                                                      write_back     => True);
      cache2 : Cache_Pointer          := Create_Cache(mem            => cache1,
                                                      line_count     => 1,
                                                      line_size      => 1,
                                                      associativity  => 1,
                                                      latency        => 1,
                                                      policy         => LRU,
                                                      write_back     => True);
   begin

      Check(Get_Cost(cache1.all) = 1);
      Check(Get_Cost(cache2.all) = 2);

      Read(cache2.all, 0, 1);
      Check(Get_Time(cache2.all) = 102);

      Read(cache2.all, 0, 1);
      Check(Get_Time(cache2.all) = 103);

      Read(cache2.all, 1, 1);
      Check(Get_Time(cache2.all) = 205);

      Read(cache2.all, 0, 1);
      Check(Get_Time(cache2.all) = 207);

      Destroy(Memory_Pointer(cache2));

   end Test_Dual;

   procedure Run_Tests is
   begin

      Test_Direct;
      Test_Set;
      Test_Dual;

   end Run_Tests;

end Test.Cache;
