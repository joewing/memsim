
with Ada.Characters.Latin_1;

package body HDL_Generator is

   type Generator_Node is record
      t        : Tag;
      ports    : Ports_Function;
      proc     : Process_Function;
   end record;

   package Generator_Vectors is new Vectors(Natural, Generator_Node);

   nodes : Generator_Vectors.Vector;

   procedure Append_Line(dest  : in out Unbounded_String;
                         str   : in String   := "";
                         shift : in Natural  := 0) is
   begin
      for i in 1 .. shift loop
         Append(dest, "   ");
      end loop;
      Append(dest, str);
      Append(dest, Ada.Characters.Latin_1.LF);
   end Append_Line;

   function Get_Ports(mem : Memory_Pointer) return Port_Type is
      result : Port_Type;
   begin
      for i in nodes.First_Index .. nodes.Last_Index loop
         declare
            node : constant Generator_Node := nodes.Element(i);
         begin
            if mem'Tag = node.t then
               return node.ports.all(mem);
            end if;
         end;
      end loop;
      return result;
   end Get_Ports;

   function Generate(mem         : Memory_Pointer;
                     name        : String;
                     addr_bits   : Positive) return String is
      mname       : constant String := "m" & To_String(Get_ID(mem.all));
      word_bits   : constant Positive := 8 * Get_Word_Size(mem.all);
      gen         : Generator_Type;
      r           : Unbounded_String;
   begin

      gen.shift := 1;
      Process(gen, mem, word_bits, addr_bits);
      Assign(gen, mname & "_addr", "addr");
      Assign(gen, mname & "_din", "din");
      Assign(gen, "dout", mname & "_dout");
      Assign(gen, mname & "_re", "re");
      Assign(gen, mname & "_we", "we");
      Assign(gen, "ready", mname & "_ready");
      gen.shift := 0;

      Append_Line(r, "library ieee;");
      Append_Line(r, "use ieee.std_logic_1164.all;");
      Append_Line(r, "use ieee.numeric_std.all;");
      Append_Line(r);
      Append_Line(r, "entity " & name & " is");
      Append_Line(r, "   generic (");
      Append_Line(r, "      ADDR_WIDTH : in natural := " &
                  To_String(addr_bits) & ";");
      Append_Line(r, "      WORD_WIDTH : in natural := " &
                  To_String(word_bits));
      Append_Line(r, "   );");
      Append_Line(r, "   port (");
      Append_Line(r, "      clk     : in  std_logic;");
      Append_Line(r, "      rst     : in  std_logic;");
      Append_Line(r, "      addr    : in  std_logic_vector(" &
                  "ADDR_WIDTH - 1 downto 0);");
      Append_Line(r, "      din     : in  std_logic_vector(" &
                  "WORD_WIDTH - 1 downto 0);");
      Append_Line(r, "      dout    : out std_logic_vector(" &
                  "WORD_WIDTH - 1 downto 0);");
      Append_Line(r, "      re      : in  std_logic;");
      Append_Line(r, "      we      : in  std_logic;");
      Append_Line(r, "      ready   : out std_logic");
      Append_Line(r, "   );");
      Append_Line(r, "end " & name & ";");
      Append_Line(r);
      Append_Line(r, "architecture " & name & "_arch of " & name & " is");
      Append(r, gen.decl);
      Append_Line(r, "begin");
      Append(r, gen.proc);
      Append_Line(r, "end " & name & "_arch;");
      return To_String(r);
   end Generate;

   procedure Process(gen         : in out Generator_Type;
                     mem         : in Memory_Pointer;
                     word_bits   : in Positive;
                     addr_bits   : in Positive) is
   begin
      for i in nodes.First_Index .. nodes.Last_Index loop
         declare
            node : constant Generator_Node := nodes.Element(i);
         begin
            if mem'Tag = node.t then
               node.proc.all(gen, mem, word_bits, addr_bits);
               return;
            end if;
         end;
      end loop;
   end Process;

   function Get_Port_Count(port : Port_Type) return Natural is
   begin
      return Natural(port.banks.Length);
   end Get_Port_Count;

   function Get_Width(port : Port_Type;
                      bank : Natural) return Positive is
   begin
      return port.banks.Element(bank);
   end Get_Width;

   procedure Add_Type(t       : in Tag;
                      ports   : in Ports_Function;
                      proc    : in Process_Function) is
   begin
      nodes.Append(Generator_Node'(t, ports, proc));
   end Add_Type;

   procedure Assign(gen    : in out Generator_Type;
                    dest   : in String;
                    src    : in String) is
   begin
      PLine(gen, dest & " <= " & src & ";");
   end Assign;

   procedure Signal(gen    : in out Generator_Type;
                    name   : in String;
                    width  : in Natural) is
      rstr : constant String := To_String(width - 1) & " downto 0";
   begin
      DLine(gen, "signal " & name & " : std_logic_vector(" & rstr & ");");
   end Signal;

   procedure Signal(gen    : in out Generator_Type;
                    name   : in String) is
   begin
      DLine(gen, "signal " & name & " : std_logic;");
   end Signal;

   procedure Declare_Signals(gen          : in out Generator_Type;
                             name         : in String;
                             word_bits    : in Positive;
                             addr_bits    : in Positive) is
   begin
      Signal(gen, name & "_addr", addr_bits);
      Signal(gen, name & "_din", word_bits);
      Signal(gen, name & "_dout", word_bits);
      Signal(gen, name & "_re");
      Signal(gen, name & "_we");
      Signal(gen, name & "_ready");
   end Declare_Signals;

   procedure DLine(gen : in out Generator_Type;
                   str : in String := "") is
   begin
      Append_Line(gen.decl, str, gen.shift);
   end DLine;

   procedure PLine(gen : in out Generator_Type;
                   str : in String := "") is
   begin
      Append_Line(gen.proc, str, gen.shift);
   end PLine;

   function To_String(a : Address_Type) return String is
      str : constant String := Address_Type'Image(a);
   begin
      if str(str'First) = ' ' then
         return str(str'First + 1 .. str'Last);
      else
         return str;
      end if;
   end To_String;

   function To_String(t : Time_Type) return String is
      str : constant String := Time_Type'Image(t);
   begin
      if str(str'First) = ' ' then
         return str(str'First + 1 .. str'Last);
      else
         return str;
      end if;
   end To_String;

end HDL_Generator;
