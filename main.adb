
with Ada.Text_IO;    use Ada.Text_IO;

with Memory;         use Memory;
with Memory.Bank;    use Memory.Bank;
with Memory.Cache;   use Memory.Cache;
with Memory.RAM;     use Memory.RAM;
with Trace;          use Trace;

procedure Main is

   ram1     : RAM_Pointer     := Create_RAM(100);
   ram2     : RAM_Pointer     := Create_RAM(100);
   bank     : Bank_Pointer    := Create_Bank;
   cache    : Cache_Pointer   := Create_Cache(bank, 4, 2, 1, 1);

begin

   Add_Bank(bank.all, ram1, 0, 1);
   Add_Bank(bank.all, ram2, 1, 1);
   Process(cache.all, "test.txt");

   Put_Line("Total time: " & Natural'image(Get_Time(cache.all)));

end Main;

