
with Memory.SPM; use Memory.SPM;
with Memory.Container; use Memory.Container;

package body HDL_Generator.SPM is

   function SPM_Ports(gen : Generator_Type;
                      mem : Memory_Pointer) return Port_Type is
      sp    : constant SPM_Pointer     := SPM_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(sp.all);
   begin
      return Get_Ports(gen, other);
   end SPM_Ports;

   function Process_SPM(gen         : Generator_Type;
                        mem         : Memory_Pointer;
                        word_bits   : Positive;
                        addr_bits   : Positive) return String is
      sp       : constant SPM_Pointer     := SPM_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(sp.all);
      name     : constant String          := To_String(Get_ID(mem.all));
      oname    : constant String          := To_String(Get_ID(other.all));
      addr_str : constant String := "[" & To_String(addr_bits - 1) & ":0]";
      word_str : constant String := "[" & To_String(word_bits - 1) & ":0]";
      size     : constant Natural := (8 * Get_Size(sp.all)) / word_bits;
      r        : Unbounded_String;
   begin
      Append(r, Generate(gen, other, word_bits, addr_bits));
      Line(r, "wire " & addr_str & " m" & name & "_addr;");
      Line(r, "wire " & word_str & " m" & name & "_din;");
      Line(r, "wire " & word_str & " m" & name & "_dout;");
      Line(r, "wire m" & name & "_re;");
      Line(r, "wire m" & name & "_we;");
      Line(r, "wire m" & name & "_ready;");
      Line(r, "spm #(.SIZE(" & To_String(size) & "), " &
              ".ADDR_WIDTH(" & To_String(addr_bits) & "), " &
              ".WORD_WIDTH(" & To_String(word_bits) & "))");
      Line(r, "   m" & name & "_inst (");
      Line(r, "   .clk(clk), .rst(rst),");
      Line(r, "   .addr(m" & name & "_addr),");
      Line(r, "   .din(m" & name & "_din),");
      Line(r, "   .dout(m" & name & "_dout),");
      Line(r, "   .re(m" & name & "_re),");
      Line(r, "   .we(m" & name & "_we),");
      Line(r, "   .ready(m" & name & "_ready),");
      Line(r, "   .maddr(m" & oname & "_addr),");
      Line(r, "   .mdin(m" & oname & "_din),");
      Line(r, "   .mdout(m" & oname & "_dout),");
      Line(r, "   .mre(m" & oname & "_re),");
      Line(r, "   .mwe(m" & oname & "_we),");
      Line(r, "   .mready(m" & oname & "_ready));");
      return To_String(r);
   end Process_SPM;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, SPM_Type'Tag, SPM_Ports'Access, Process_SPM'Access);
   end Register;

end HDL_Generator.SPM;
