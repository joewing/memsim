
with Memory.Split;      use Memory.Split;
with Memory.Container;  use Memory.Container;

package body HDL_Generator.Split is

   function Split_Ports(gen : Generator_Type;
                        mem : Memory_Pointer) return Port_Type is
      sp    : constant Split_Pointer   := Split_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(sp.all);
   begin
      return Get_Ports(gen, other);
   end Split_Ports;

   function Process_Split(gen       : Generator_Type;
                          mem       : Memory_Pointer;
                          word_bits : Positive;
                          addr_bits : Positive) return String is
      sp       : constant Split_Pointer   := Split_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(sp.all);
      bank0    : constant Memory_Pointer  := Get_Bank(sp, 0);
      bank1    : constant Memory_Pointer  := Get_Bank(sp, 1);
      name     : constant String := "m" & To_String(Get_ID(mem.all));
      oname    : constant String := "m" & To_String(Get_ID(other.all));
      b0name   : constant String := "m" & To_String(Get_ID(bank0.all));
      b1name   : constant String := "m" & To_String(Get_ID(bank1.all));
      addr_str : constant String := "[" & To_String(addr_bits - 1) & ":0]";
      word_str : constant String := "[" & To_String(word_bits - 1) & ":0]";
      offset   : constant Address_Type := Get_Offset(sp.all);
      r        : Unbounded_String;
   begin
      Append(r, Generate(gen, other, word_bits, addr_bits));
      Append(r, Generate(gen, bank0, word_bits, addr_bits));
      Append(r, Generate(gen, bank1, word_bits, addr_bits));
      Line(r, "wire " & addr_str & " " & name & "_addr;");
      Line(r, "wire " & word_str & " " & name & "_din;");
      Line(r, "wire " & word_str & " " & name & "_dout;");
      Line(r, "wire " & name & "_re;");
      Line(r, "wire " & name & "_we;");
      Line(r, "wire " & name & "_ready;");
      Line(r, "split #(.OFFSET(" & To_String(offset) & "), " &
              ".ADDR_WIDTH(" & To_String(addr_bits) & "), " &
              ".WORD_WIDTH(" & To_String(word_bits) & "))");
      Line(r, "   " & name & "_inst (");
      Line(r, "   .clk(clk), .rst(rst),");
      Line(r, "   .addr(m" & name & "_addr),");
      Line(r, "   .din(m" & name & "_din),");
      Line(r, "   .dout(m" & name & "_dout),");
      Line(r, "   .re(m" & name & "_re),");
      Line(r, "   .we(m" & name & "_we),");
      Line(r, "   .ready(m" & name & "_ready),");
      return To_String(r);
   end Process_Split;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, Split_Type'Tag, Split_Ports'Access,
               Process_Split'Access);
   end Register;

end HDL_Generator.Split;
