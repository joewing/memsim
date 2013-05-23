
with Memory.Cache;      use Memory.Cache;
with Memory.Container;  use Memory.Container;
with Util;              use Util;

package body HDL_Generator.Cache is

   function Cache_Ports(mem : Memory_Pointer) return Port_Type is
      cp    : constant Cache_Pointer   := Cache_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(cp.all);
   begin
      return Get_Ports(other);
   end Cache_Ports;

   procedure Process_Cache(gen         : in out Generator_Type;
                           mem         : in Memory_Pointer;
                           word_bits   : in Positive;
                           addr_bits   : in Positive) is
      cp       : constant Cache_Pointer   := Cache_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(cp.all);
      name     : constant String    := "m" & To_String(Get_ID(mem.all));
      oname    : constant String    := "m" & To_String(Get_ID(other.all));
      lsize    : constant Positive  := 8 * Get_Line_Size(cp.all) / word_bits;
      lcount   : constant Positive  := Get_Line_Count(cp.all);
      assoc    : constant Natural   := Get_Associativity(cp.all);
   begin
      Process(gen, other, word_bits, addr_bits);
      Declare_Signals(gen, name, word_bits, addr_bits);
      PLine(gen, name & "_inst : entity work.cache");
      PLine(gen, "   generic map (");
      PLine(gen, "      ADDR_WIDTH      => " & To_String(addr_bits) & ",");
      PLine(gen, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      PLine(gen, "      LINE_SIZE_BITS  => " &
            To_String(Log2(lsize - 1)) & ",");
      PLine(gen, "      LINE_COUNT_BITS => " &
            To_String(Log2(lcount - 1)) & ",");
      PLine(gen, "      ASSOC_BITS      => " & To_String(Log2(assoc - 1)));
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
   end Process_Cache;

   procedure Register is
   begin
      Add_Type(Cache_Type'Tag, Cache_Ports'Access, Process_Cache'Access);
   end Register;

end HDL_Generator.Cache;
