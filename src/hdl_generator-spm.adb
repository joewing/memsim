
with Memory.SPM; use Memory.SPM;
with Memory.Container; use Memory.Container;

package body HDL_Generator.SPM is

   function SPM_Ports(mem : Memory_Pointer) return Port_Type is
      sp    : constant SPM_Pointer     := SPM_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(sp.all);
   begin
      return Get_Ports(other);
   end SPM_Ports;

   procedure Process_SPM(gen        : in out Generator_Type;
                         mem        : in Memory_Pointer;
                         word_bits  : in Positive;
                         addr_bits  : in Positive) is
      sp       : constant SPM_Pointer     := SPM_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(sp.all);
      name     : constant String    := "m" & To_String(Get_ID(mem.all));
      oname    : constant String    := "m" & To_String(Get_ID(other.all));
      size     : constant Natural   := (8 * Get_Size(sp.all)) / word_bits;
   begin
      Process(gen, other, word_bits, addr_bits);
      Declare_Signals(gen, name, word_bits, addr_bits);
      PLine(gen, name & "_inst : entity work.spm");
      PLine(gen, "   generic map (");
      PLine(gen, "      ADDR_WIDTH => " & To_String(addr_bits) & ",");
      PLine(gen, "      WORD_WIDTH => " & To_String(word_bits) & ",");
      PLine(gen, "      SIZE       => " & To_String(size));
      PLine(gen, "   )");
      PLine(gen, "   port map (");
      PLine(gen, "      clk      => clk,");
      PLine(gen, "      rst      => rst,");
      PLine(gen, "      addr     => " & name & "_addr,");
      PLine(gen, "      din      => " & name & "_din,");
      PLine(gen, "      dout     => " & name & "_dout,");
      PLine(gen, "      re       => " & name & "_re,");
      PLine(gen, "      we       => " & name & "_we,");
      PLine(gen, "      ready    => " & name & "_ready,");
      PLine(gen, "      maddr    => " & oname & "_addr,");
      PLine(gen, "      min      => " & oname & "_dout,");
      PLine(gen, "      mout     => " & oname & "_din,");
      PLine(gen, "      mre      => " & oname & "_re,");
      PLine(gen, "      mwe      => " & oname & "_we,");
      PLine(gen, "      mready   => " & oname & "_ready");
      PLine(gen, "   );");
   end Process_SPM;

   procedure Register is
   begin
      Add_Type(SPM_Type'Tag, SPM_Ports'Access, Process_SPM'Access);
   end Register;

end HDL_Generator.SPM;
