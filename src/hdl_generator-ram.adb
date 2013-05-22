
with Ada.Text_IO; use Ada.Text_IO;
with Memory.RAM; use Memory.RAM;

package body HDL_Generator.RAM is

   function RAM_Ports(mem : Memory_Pointer) return Port_Type is
      ram      : constant RAM_Pointer := RAM_Pointer(mem);
      result   : Port_Type;
   begin
      result.banks.Append(Get_Word_Size(ram.all));
      return result;
   end RAM_Ports;

   procedure Process_RAM(gen        : in out Generator_Type;
                         mem        : in Memory_Pointer;
                         word_bits  : in Positive;
                         addr_bits  : in Positive) is
      rp       : constant RAM_Pointer := RAM_Pointer(mem);
      name     : constant String := "m" & To_String(Get_ID(mem.all));
      words    : constant Positive := Get_Word_Count(rp.all);
      latency  : constant Time_Type := Get_Latency(rp.all);
   begin
      Declare_Signals(gen, name, word_bits, addr_bits);
      PLine(gen, name & "_inst : entity work.ram");
      PLine(gen, "   generic map (");
      PLine(gen, "      ADDR_WIDTH      => " & To_String(addr_bits) & ",");
      PLine(gen, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      PLine(gen, "      SIZE            => " & To_String(words) & ",");
      PLine(gen, "      LATENCY         => " & To_String(latency));
      PLine(gen, "   )");
      PLine(gen, "   port map (");
      PLine(gen, "      clk      => clk,");
      PLine(gen, "      rst      => rst,");
      PLine(gen, "      addr     => " & name & "_addr,");
      PLine(gen, "      din      => " & name & "_din,");
      PLine(gen, "      dout     => " & name & "_dout,");
      PLine(gen, "      re       => " & name & "_re,");
      PLine(gen, "      we       => " & name & "_we,");
      PLine(gen, "      ready    => " & name & "_ready");
      PLine(gen, "   );");
   end Process_RAM;

   procedure Register is
   begin
      Add_Type(RAM_Type'Tag, RAM_Ports'Access, Process_RAM'Access);
   end Register;

end HDL_Generator.RAM;
