
with Memory;   use Memory;
with Trace;    use Trace;
with Parser;

procedure Main is

   mem      : Memory_Pointer := null;

begin

   mem := Parser.Parse("memory.txt");
   if mem /= null then
      Process(mem.all, "trace.txt", 50);
      Destroy(mem);
   end if;

end Main;

