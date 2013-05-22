
with Memory.Split;      use Memory.Split;
with Memory.Join;       use Memory.Join;
with Memory.Container;  use Memory.Container;

package body HDL_Generator.Split is

   function Split_Ports(mem : Memory_Pointer) return Port_Type is
      sp    : constant Split_Pointer   := Split_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(sp.all);
   begin
      return Get_Ports(other);
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

   procedure Process_Split(gen         : in out Generator_Type;
                           mem         : in Memory_Pointer;
                           word_bits   : in Positive;
                           addr_bits   : in Positive) is
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
      offset   : constant Address_Type := Get_Offset(sp.all);
   begin

      Process(gen, other, word_bits, addr_bits);

      -- Port into bank0 is b0name.
      -- Port out of bank0 is out0name.
      Process(gen, bank0, word_bits, addr_bits);

      -- Port into bank1 is b1name.
      -- Port out of bank1 is out1name.
      Process(gen, bank1, word_bits, addr_bits);

      PLine(gen, name & "_combine : entity work.combine");
      PLine(gen, "   generic map (");
      PLine(gen, "      ADDR_WIDTH      => " & To_String(addr_bits) & ",");
      PLine(gen, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      PLine(gen, "      OFFSET          => " & To_String(offset));
      PLine(gen, "   )");
      PLine(gen, "   port map (");
      PLine(gen, "      clk      => clk,");
      PLine(gen, "      rst      => rst,");
      PLine(gen, "      addr0    => " & out0name & "_addr,");
      PLine(gen, "      din0     => " & out0name & "_din,");
      PLine(gen, "      dout0    => " & out0name & "_dout,");
      PLine(gen, "      re0      => " & out0name & "_re,");
      PLine(gen, "      we0      => " & out0name & "_we,");
      PLine(gen, "      ready0   => " & out0name & "_ready,");
      PLine(gen, "      addr1    => " & out1name & "_addr,");
      PLine(gen, "      din1     => " & out1name & "_din,");
      PLine(gen, "      dout1    => " & out1name & "_dout,");
      PLine(gen, "      re1      => " & out1name & "_re,");
      PLine(gen, "      we1      => " & out1name & "_we,");
      PLine(gen, "      ready1   => " & out1name & "_ready,");
      PLine(gen, "      maddr    => " & oname & "_addr,");
      PLine(gen, "      mout     => " & oname & "_din,");
      PLine(gen, "      min      => " & oname & "_dout,");
      PLine(gen, "      mre      => " & oname & "_re,");
      PLine(gen, "      mwe      => " & oname & "_we,");
      PLine(gen, "      mready   => " & oname & "_ready");
      PLine(gen, "   );");

      -- Port into the split.
      Declare_Signals(gen, name, word_bits, addr_bits);

      PLine(gen, name & "_sp : entity work.split");
      PLine(gen, "   generic map (");
      PLine(gen, "      ADDR_WIDTH      => " & To_String(addr_bits) & ",");
      PLine(gen, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      PLine(gen, "      OFFSET          => " & To_String(offset));
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
      PLine(gen, "      maddr0   => " & b0name & "_addr,");
      PLine(gen, "      mout0    => " & b0name & "_din,");
      PLine(gen, "      min0     => " & b0name & "_dout,");
      PLine(gen, "      mre0     => " & b0name & "_re,");
      PLine(gen, "      mwe0     => " & b0name & "_we,");
      PLine(gen, "      mready0  => " & b0name & "_ready,");
      PLine(gen, "      maddr1   => " & b1name & "_addr,");
      PLine(gen, "      mout1    => " & b1name & "_din,");
      PLine(gen, "      min1     => " & b1name & "_dout,");
      PLine(gen, "      mre1     => " & b1name & "_re,");
      PLine(gen, "      mwe1     => " & b1name & "_we,");
      PLine(gen, "      mready1  => " & b1name & "_ready");
      PLine(gen, "   );");

   end Process_Split;

   procedure Register is
   begin
      Add_Type(Split_Type'Tag, Split_Ports'Access, Process_Split'Access);
   end Register;

end HDL_Generator.Split;
