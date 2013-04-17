
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

   Put_Line("module memory(clk, rst, addr, din, dout, re, we, ready);");
   New_Line;
   Put_Line("   input wire clk;");
   Put_Line("   input wire rst;");
   Put_Line("   input wire [63:0] addr;");
   Put_Line("   input wire [63:0] din;");
   Put_Line("   output wire [63:0] dout;");
   Put_Line("   input wire re;");
   Put_Line("   input wire we;");
   Put_Line("   output wire ready;");
   New_Line;
   Put_Line(Generate(gen, mem, 64, 64));
   declare
      id       : constant Natural   := Get_ID(mem.all);
      id_str   : constant String    := Natural'Image(id);
      name     : constant String
                  := "m" & id_str(id_str'First + 1 .. id_str'Last);
   begin
      Put_Line("   assign " & name & "_addr = addr;");
      Put_Line("   assign " & name & "_din = din;");
      Put_Line("   assign dout = " & name & "_dout;");
      Put_Line("   assign " & name & "_re = re;");
      Put_Line("   assign " & name & "_we = we;");
      Put_Line("   assign ready = " & name & "_ready;");
   end;
   New_Line;
   Put_Line("endmodule");
   Destroy(mem);

end MemGen;
