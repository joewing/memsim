
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
      name     : constant String          := Get_ID(gen);
      r        : Unbounded_String;
   begin
      Line(r, "module spm_" & name & "(clk, rst,");
      Line(r, "   addr, din, dout, re, we,");
      Line(r, "   maddr, min, mout, mre, mwe);");
      Line(r);
      Line(r, "   input wire [" & To_String(addr_bits - 1) & ":0] addr;");
      Line(r, "   input wire [" & To_String(word_bits - 1) & ":0] din;");
      Line(r, "   output wire [" & To_String(word_bits - 1) & ":0] dout;");
      Line(r, "   input wire re;");
      Line(r, "   input wire we;");
      Line(r);
      Line(r);
      Line(r, "endmodule");
      return To_String(r);
   end Process_SPM;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, SPM_Type'Tag, SPM_Ports'Access, Process_SPM'Access);
   end Register;

end HDL_Generator.SPM;
