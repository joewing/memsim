
with Memory;            use Memory;
with Memory.Bank;       use Memory.Bank;
with Memory.Cache;      use Memory.Cache;
with Memory.RAM;        use Memory.RAM;
with Memory.Prefetch;   use Memory.Prefetch;
with Trace;             use Trace;

with Parser;

procedure Main is

   ram1     : RAM_Pointer        := Create_RAM(100);
   ram2     : RAM_Pointer        := Create_RAM(100);
   bank     : Bank_Pointer       := Create_Bank;
   cache    : Cache_Pointer      := Create_Cache(bank, 4, 1, 1, 1);
   prefetch : Prefetch_Pointer   := Create_Prefetch(cache, 0, 2);
   mem      : Memory_Pointer;

begin

   mem := Parser.Parse("bah.txt");

   Add_Bank(bank.all, ram1, 0, 1);
   Add_Bank(bank.all, ram2, 1, 1);
   Process(prefetch.all, "test.txt", 50);

end Main;

