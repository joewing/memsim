
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;
with Util; use Util;

package body HDL_Generator is

   procedure Line(dest  : in out Unbounded_String;
                  str   : in String   := "") is
   begin
      Append(dest, str);
      Append(dest, Ada.Characters.Latin_1.LF);
   end Line;

   function Generate(mem         : Memory_Pointer;
                     name        : String;
                     addr_bits   : Positive) return String is
      mname       : constant String := "m" & To_String(Get_ID(mem.all));
      word_bits   : constant Positive := 8 * Get_Word_Size(mem.all);
      sigs        : Unbounded_String;
      code        : Unbounded_String;
      r           : Unbounded_String;
   begin

      Generate(mem.all, sigs, code);

      Line(code, "   " & mname & "_addr <= addr;");
      Line(code, "   " & mname & "_din <= din;");
      Line(code, "   dout <= " & mname & "_dout;");
      Line(code, "   " & mname & "_re <= re;");
      Line(code, "   " & mname & "_we <= we;");
      Line(code, "   ready <= " & mname & "_ready;");

      Line(r, "library ieee;");
      Line(r, "use ieee.std_logic_1164.all;");
      Line(r, "use ieee.numeric_std.all;");
      Line(r);
      Line(r, "entity " & name & " is");
      Line(r, "   generic (");
      Line(r, "      ADDR_WIDTH : in natural := " &
              To_String(addr_bits) & ";");
      Line(r, "      WORD_WIDTH : in natural := " &
              To_String(word_bits));
      Line(r, "   );");
      Line(r, "   port (");
      Line(r, "      clk     : in  std_logic;");
      Line(r, "      rst     : in  std_logic;");
      Line(r, "      addr    : in  std_logic_vector(" &
              "ADDR_WIDTH - 1 downto 0);");
      Line(r, "      din     : in  std_logic_vector(" &
              "WORD_WIDTH - 1 downto 0);");
      Line(r, "      dout    : out std_logic_vector(" &
              "WORD_WIDTH - 1 downto 0);");
      Line(r, "      re      : in  std_logic;");
      Line(r, "      we      : in  std_logic;");
      Line(r, "      ready   : out std_logic");
      Line(r, "   );");
      Line(r, "end " & name & ";");
      Line(r);
      Line(r, "architecture " & name & "_arch of " & name & " is");
      Append(r, sigs);
      Line(r, "begin");
      Append(r, code);
      Line(r, "end " & name & "_arch;");

      return To_String(r);
   end Generate;

end HDL_Generator;
