
with Memory;
with Memory.Bank;
with Memory.Cache;
with Memory.RAM;
with Trace;

procedure Main is

   cache    : Memory.Cache.Cached_Pointer := new Memory.Cache.Cached_Memory;
   mem      : Memory.Bank.Banked_Pointer  := new Memory.Bank.Banked_Memory;
   bank1    : Memory.RAM.RAM_Pointer      := new Memory.RAM.RAM;
   bank2    : Memory.RAM.RAM_Pointer      := new Memory.RAM.RAM;

begin

   Memory.RAM.Set_Latency(bank1, 100);
   Memory.RAM.Set_Latency(bank2, 100);
   Memory.Bank.Add_Bank(mem, Memory.Memory_Pointer(bank1), 0, 1);
   Memory.Bank.Add_Bank(mem, Memory.Memory_Pointer(bank2), 1, 1);
   Memory.Cache.Set_Memory(cache, Memory.Memory_Pointer(mem));
   Memory.Cache.Set_Line_Size(cache, 2);
   Memory.Cache.Set_Line_Count(cache, 4);
   Memory.Cache.Set_Associativity(cache, 2);
   Trace.Process(Memory.Memory_Pointer(cache), "test.txt");

end Main;

