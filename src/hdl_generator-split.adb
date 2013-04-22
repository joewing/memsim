
with Memory.Split;      use Memory.Split;
with Memory.Join;       use Memory.Join;
with Memory.Container;  use Memory.Container;

package body HDL_Generator.Split is

   function Split_Ports(gen : Generator_Type;
                        mem : Memory_Pointer) return Port_Type is
      sp    : constant Split_Pointer   := Split_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(sp.all);
   begin
      return Get_Ports(gen, other);
   end Split_Ports;

   function Find_Join(mem : Memory_Pointer) return Join_Pointer is
   begin
      if mem.all in Join_Type'Class then
         return Join_Pointer(mem);
      else
         declare
            cp : constant Container_Pointer := Container_Pointer(mem);
         begin
            return Find_Join(Get_Memory(cp.all));
         end;
      end if;
   end Find_Join;

   function Process_Split(gen       : Generator_Type;
                          mem       : Memory_Pointer;
                          word_bits : Positive;
                          addr_bits : Positive) return String is
      sp       : constant Split_Pointer   := Split_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(sp.all);
      bank0    : constant Memory_Pointer  := Get_Bank(sp, 0);
      bank1    : constant Memory_Pointer  := Get_Bank(sp, 1);
      join0    : constant Join_Pointer    := Find_Join(bank0);
      join1    : constant Join_Pointer    := Find_Join(bank1);
      name     : constant String := "m" & To_String(Get_ID(mem.all));
      oname    : constant String := "m" & To_String(Get_ID(other.all));
      b0name   : constant String := "m" & To_String(Get_ID(bank0.all));
      b1name   : constant String := "m" & To_String(Get_ID(bank1.all));
      out0name : constant String := "m" & To_String(Get_ID(join0.all));
      out1name : constant String := "m" & To_String(Get_ID(join1.all));
      addr_str : constant String := "[" & To_String(addr_bits - 1) & ":0]";
      word_str : constant String := "[" & To_String(word_bits - 1) & ":0]";
      offset   : constant Address_Type := Get_Offset(sp.all);
      r        : Unbounded_String;
   begin

      Append(r, Generate(gen, other, word_bits, addr_bits));

      -- Port into bank0 is b0name.
      -- Port out of bank0 is out0name.
      Append(r, Generate(gen, bank0, word_bits, addr_bits));

      -- Port into bank1 is b1name.
      -- Port out of bank1 is out1name.
      Append(r, Generate(gen, bank1, word_bits, addr_bits));

      Line(r, "combine #(.OFFSET(" & To_String(offset) & "), " &
              ".ADDR_WIDTH(" & To_String(addr_bits) & "), " &
              ".WORD_WIDTH(" & To_String(word_bits) & "))");
      Line(r, "   " & out0name & "_inst (");
      Line(r, "   .clk(clk), .rst(rst),");
      Line(r, "   .addr0(" & out0name & "_addr),");
      Line(r, "   .din0(" & out0name & "_din),");
      Line(r, "   .dout0(" & out0name & "_dout),");
      Line(r, "   .re0(" & out0name & "_re),");
      Line(r, "   .we0(" & out0name & "_we),");
      Line(r, "   .ready0(" & out0name & "_ready),");
      Line(r, "   .addr1(" & out1name & "_addr),");
      Line(r, "   .din1(" & out1name & "_din),");
      Line(r, "   .dout1(" & out1name & "_dout),");
      Line(r, "   .re1(" & out1name & "_re),");
      Line(r, "   .we1(" & out1name & "_we),");
      Line(r, "   .ready1(" & out1name & "_ready),");
      Line(r, "   .maddr(" & oname & "_addr),");
      Line(r, "   .mout(" & oname & "_din),");
      Line(r, "   .min(" & oname & "_dout),");
      Line(r, "   .mre(" & oname & "_re),");
      Line(r, "   .mwe(" & oname & "_we),");
      Line(r, "   .mready(" & oname & "_ready));");

      -- Port into the split.
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
      Line(r, "   .addr(" & name & "_addr),");
      Line(r, "   .din(" & name & "_din),");
      Line(r, "   .dout(" & name & "_dout),");
      Line(r, "   .re(" & name & "_re),");
      Line(r, "   .we(" & name & "_we),");
      Line(r, "   .ready(" & name & "_ready),");
      Line(r, "   .maddr0(" & b0name & "_addr),");
      Line(r, "   .mout0(" & b0name & "_din),");
      Line(r, "   .min0(" & b0name & "_dout),");
      Line(r, "   .mre0(" & b0name & "_re),");
      Line(r, "   .mwe0(" & b0name & "_we),");
      Line(r, "   .mready0(" & b0name & "_ready),");
      Line(r, "   .maddr1(" & b1name & "_addr),");
      Line(r, "   .mout1(" & b1name & "_din),");
      Line(r, "   .min1(" & b1name & "_dout),");
      Line(r, "   .mre1(" & b1name & "_re),");
      Line(r, "   .mwe1(" & b1name & "_we),");
      Line(r, "   .mready1(" & b1name & "_ready));");
      return To_String(r);
   end Process_Split;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, Split_Type'Tag, Split_Ports'Access,
               Process_Split'Access);
   end Register;

end HDL_Generator.Split;
