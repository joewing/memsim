
with Memory.RAM;        use Memory.RAM;
with Memory.Cache;      use Memory.Cache;
with Memory.Prefetch;   use Memory.Prefetch;

package body Test.Prefetch is

   procedure Run_Tests is

      ram : constant RAM_Pointer := Create_RAM(latency   => 100,
                                               word_size => 8);
      cache : constant Cache_Pointer := Create_Cache(mem             => ram,
                                                     line_count      => 1,
                                                     line_size       => 8,
                                                     associativity   => 1,
                                                     latency         => 1,
                                                     policy          => LRU,
                                                     write_back      => True);
      prefetch : Prefetch_Pointer := Create_Prefetch(cache, 8);

   begin

      Check(Get_Time(prefetch.all) = 0);
      Check(Get_Cost(prefetch.all) = Get_Cost(cache.all));

      Read(prefetch.all, 0, 8);
      Check(Get_Time(prefetch.all) = 202);
      Check(Get_Time(ram.all) = 200);

      Read(prefetch.all, 8, 8);
      Check(Get_Time(prefetch.all) = 304);
      Check(Get_Time(ram.all) = 300);

      Destroy(Memory_Pointer(prefetch));

   end Run_Tests;

end Test.Prefetch;
