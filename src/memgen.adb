
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Memory;                  use Memory;
with Parser;                  use Parser;
with HDL_Generator;           use HDL_Generator;

procedure MemGen is

   mem         : Memory_Pointer;
   name        : Unbounded_String := To_Unbounded_String("mem");
   addr_width  : Positive := 32;
   mem_index   : Integer := -1;

   procedure Show_Usage is
   begin
      Put_Line("usage: " & Command_Name & " [options] <memory>");
      Put_Line("options:");
      Put_Line("  -width <int>   Address width (default" &
               Positive'Image(addr_width) & ")");
   end Show_Usage;

begin

   declare
      i     : Positive := 1;
      arg   : Unbounded_String;
   begin
      while i <= Argument_Count loop
         arg := To_Unbounded_String(Argument(i));
         if arg = "-width" and i + 1 <= Argument_Count then
            i := i + 1;
            addr_width := Positive'Value(Argument(i));
         elsif arg = "-name" and i + 1 <= Argument_Count then
            i := i + 1;
            name := To_Unbounded_String(Argument(i));
         elsif mem_index < 0 then
            mem_index := i;
         else
            Show_Usage;
            return;
         end if;
         i := i + 1;
      end loop;
   end;
   if mem_index < 0 then
      Show_Usage;
      return;
   end if;

   mem := Parser.Parse(Argument(1));
   if mem = null then
      Put_Line("error: could not open memory: " & Argument(1));
      return;
   end if;

   Put_Line(Generate(mem, To_String(name), addr_width));

   Destroy(mem);

end MemGen;
