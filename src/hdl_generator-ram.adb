
with Memory.RAM; use Memory.RAM;

package body HDL_Generator.RAM is

   function RAM_Ports(gen : Generator_Type;
                      mem : Memory_Pointer) return Port_Type is
      ram      : constant RAM_Pointer := RAM_Pointer(mem);
      result   : Port_Type;
   begin
      result.banks.Append(Get_Word_Size(ram.all));
      return result;
   end RAM_Ports;

   function Process_RAM(gen         : Generator_Type;
                        mem         : Memory_Pointer;
                        word_bits   : Positive;
                        addr_bits   : Positive) return String is
      name     : constant String := "m" & To_String(Get_ID(mem.all));
      addr_str : constant String := "[" & To_String(addr_bits - 1) & ":0]";
      word_str : constant String := "[" & To_String(word_bits - 1) & ":0]";
      r        : Unbounded_String;
   begin
      Line(r, "wire " & addr_str & " " & name & "_addr;");
      Line(r, "wire " & word_str & " " & name & "_din;");
      Line(r, "wire " & word_str & " " & name & "_dout;");
      Line(r, "wire " & name & "_re;");
      Line(r, "wire " & name & "_we;");
      Line(r, "wire " & name & "_ready;");
      Line(r, "ram #(.ADDR_WIDTH(" & To_String(addr_bits) & "), " &
              ".WORD_WIDTH(" & To_String(word_bits) & "))");
      Line(r, "   " & name & "_inst (");
      Line(r, "   .clk(clk), .rst(rst),");
      Line(r, "   .addr(" & name & "_addr),");
      Line(r, "   .din(" & name & "_din),");
      Line(r, "   .dout(" & name & "_dout),");
      Line(r, "   .re(" & name & "_re),");
      Line(r, "   .we(" & name & "_we),");
      Line(r, "   .ready(" & name & "_ready));");
      return To_String(r);
   end Process_RAM;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, RAM_Type'Tag, RAM_Ports'Access, Process_RAM'Access);
   end Register;

end HDL_Generator.RAM;
