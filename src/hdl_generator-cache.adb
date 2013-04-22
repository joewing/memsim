
with Memory.Cache;      use Memory.Cache;
with Memory.Container;  use Memory.Container;

package body HDL_Generator.Cache is

   function Cache_Ports(gen : Generator_Type;
                        mem : Memory_Pointer) return Port_Type is
      cp    : constant Cache_Pointer   := Cache_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(cp.all);
   begin
      return Get_Ports(gen, other);
   end Cache_Ports;

   function Process_Cache(gen       : Generator_Type;
                          mem       : Memory_Pointer;
                          word_bits : Positive;
                          addr_bits : Positive) return String is
      cp       : constant Cache_Pointer   := Cache_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(cp.all);
      name     : constant String          := To_String(Get_ID(mem.all));
      oname    : constant String          := To_String(Get_ID(other.all));
      addr_str : constant String := "[" & To_String(addr_bits - 1) & ":0]";
      word_str : constant String := "[" & To_String(word_bits - 1) & ":0]";
      lsize    : constant Positive := 8 * Get_Line_Size(cp.all) / word_bits;
      lcount   : constant Positive := Get_Line_Count(cp.all);
      assoc    : constant Positive := Get_Associativity(cp.all);
      r        : Unbounded_String;
   begin
      Append(r, Generate(gen, other, word_bits, addr_bits));
      Line(r, "wire " & addr_str & " m" & name & "_addr;");
      Line(r, "wire " & word_str & " m" & name & "_din;");
      Line(r, "wire " & word_str & " m" & name & "_dout;");
      Line(r, "wire m" & name & "_re;");
      Line(r, "wire m" & name & "_we;");
      Line(r, "wire m" & name & "_ready;");
      Line(r, "cache #(.ADDR_WIDTH(" & To_String(addr_bits) & "), " &
              ".WORD_WIDTH(" & To_String(word_bits) & "), " &
              ".LINE_SIZE(" & To_String(lsize) & "), " &
              ".LINE_COUNT(" & To_String(lcount) & "), " &
              ".ASSOCATIVITY(" & To_String(assoc) & "))");
      Line(r, "   m" & name & "_inst (");
      Line(r, "   .clk(clk), .rst(rst),");
      Line(r, "   .addr(m" & name & "_addr),");
      Line(r, "   .din(m" & name & "_din),");
      Line(r, "   .dout(m" & name & "_dout),");
      Line(r, "   .re(m" & name & "_re),");
      Line(r, "   .we(m" & name & "_we),");
      Line(r, "   .ready(m" & name & "_ready),");
      Line(r, "   .maddr(m" & oname & "_addr),");
      Line(r, "   .min(m" & oname & "_din),");
      Line(r, "   .mout(m" & oname & "_dout),");
      Line(r, "   .mre(m" & oname & "_re),");
      Line(r, "   .mwe(m" & oname & "_we),");
      Line(r, "   .mready(m" & oname & "_ready));");
      return To_String(r);
   end Process_Cache;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, Cache_Type'Tag, Cache_Ports'Access, Process_Cache'Access);
   end Register;

end HDL_Generator.Cache;
