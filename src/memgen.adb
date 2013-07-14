
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Memory;                  use Memory;
with Parser;                  use Parser;
with HDL_Generator;           use HDL_Generator;
with Simplify_Memory;

procedure MemGen is

   mem         : Memory_Pointer;
   name        : Unbounded_String := To_Unbounded_String("mem");
   addr_width  : Positive := 32 - 3;
   mem_index   : Integer := -1;
   simplify    : Boolean := True;

   procedure Show_Usage is
   begin
      Put_Line("usage: " & Command_Name & " [options] <memory>");
      Put_Line("options:");
      Put_Line("  -width <int>   Address width (default" &
               Positive'Image(addr_width) & ")");
      Put_Line("  -nosimplify    Disable memory simplification");
   end Show_Usage;

begin

   -- Parse arguments.
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
         elsif arg = "-nosimplify" then
            simplify := False;
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

   -- Parse the memory subsystem.
   mem := Parser.Parse(Argument(mem_index));
   if mem = null then
      Put_Line("error: could not open memory: " & Argument(1));
      return;
   end if;

   -- Simplify the memory subsystem if desired.
   Put_Line("-- Input: " & To_String(To_String(mem.all)));
   if simplify then
      mem := Simplify_Memory(mem);
      Put_Line("-- Simplified: " & To_String(To_String(mem.all)));
   end if;

   -- Output the VHDL for the memory subsystem.
   Put_Line(Generate(mem, To_String(name), addr_width));

   -- Clean up.
   Destroy(mem);

end MemGen;
