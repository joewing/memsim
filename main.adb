
with Clock;
with Memory;
with Memory.Bank;
with Memory.Cache;
with Memory.RAM;
with Trace;

procedure Main is

   clk      : Clock.Clock_Pointer         := new Clock.Clock_Type;
   cache    : Memory.Cache.Cache_Pointer;
   mem      : Memory.Bank.Bank_Pointer;
   bank1    : Memory.RAM.RAM_Pointer;
   bank2    : Memory.RAM.RAM_Pointer;

begin

   mem := Memory.Bank.Create_Bank(clk);
   Memory.Bank.Add_Bank(mem, Memory.Memory_Pointer(bank1), 0, 1);
   Memory.Bank.Add_Bank(mem, Memory.Memory_Pointer(bank2), 1, 1);

   cache := Memory.Cache.Create_Cache(clk, 4, 2, 1);

   Memory.Cache.Set_Memory(cache, Memory.Memory_Pointer(mem));
   Memory.Cache.Set_Line_Size(cache, 2);
   Memory.Cache.Set_Line_Count(cache, 4);
   Memory.Cache.Set_Associativity(cache, 2);
   Trace.Process(Memory.Memory_Pointer(cache), "test.txt");

end Main;

