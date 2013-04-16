
with Ada.Command_Line;     use Ada.Command_Line;
with Ada.Text_IO;          use Ada.Text_IO;
with Memory;               use Memory;
with Parser;               use Parser;

with HDL_Generator;        use HDL_Generator;
with HDL_Generator.SPM;
with HDL_Generator.RAM;

procedure MemGen is

   gen : Generator_Type;
   mem : Memory_Pointer;

begin

   if Argument_Count /= 1 then
      Put_Line("usage: " & Command_Name & " <memory>");
      return;
   end if;

   mem := Parser.Parse(Argument(1));
   if mem = null then
      Put_Line("error: could not open memory: " & Argument(1));
      return;
   end if;

   RAM.Register(gen);
   SPM.Register(gen);

   Put_Line(Generate(gen, mem));
   Destroy(mem);

end MemGen;
