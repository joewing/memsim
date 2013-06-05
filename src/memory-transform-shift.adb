
with Memory.Join; use Memory.Join;
with Util;        use Util;

package body Memory.Transform.Shift is

   function Create_Shift return Shift_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      return result;
   end Create_Shift;

   function Random_Shift(next       : access Memory_Type'Class;
                         generator  : RNG.Generator;
                         max_cost   : Cost_Type) return Memory_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      Set_Memory(result.all, next);
      result.shift := (RNG.Random(generator) mod 16) + 1;
      return Memory_Pointer(result);
   end Random_Shift;

   function Get_Shift(mem : Shift_Type) return Integer is
   begin
      return mem.shift;
   end Get_Shift;

   procedure Set_Shift(mem    : in out Shift_Type;
                       shift  : in Integer) is
      abits    : constant Integer := Address_Type'Size;  -- FIXME
      wsize    : constant Integer := Get_Word_Size(mem);
      wbits    : constant Integer := Log2(wsize);
   begin
      if shift < 0 then
         mem.shift := (abits - wbits + shift + 1) mod Address_Type'Size;
      else
         mem.shift := shift mod Address_Type'Size;
      end if;
   end Set_Shift;

   function Clone(mem : Shift_Type) return Memory_Pointer is
      result : constant Shift_Pointer := new Shift_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Shift_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      if mem.shift = 0 then
         mem.shift := mem.shift + 1;
      elsif mem.shift = Address_Type'Size then
         mem.shift := mem.shift - 1;
      elsif (RNG.Random(generator) mod 2) = 0 then
         mem.shift := mem.shift + 1;
      else
         mem.shift := mem.shift - 1;
      end if;
   end Permute;

   function Apply(mem      : Shift_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      abits    : constant Integer      := Address_Type'Size;   -- FIXME
      shift    : constant Integer      := mem.shift;
      wsize    : constant Address_Type := Address_Type(Get_Word_Size(mem));
      caddr    : constant Address_Type := address mod wsize;
      saddr    : constant Address_Type := address / wsize;
      rmult    : constant Address_Type := Address_Type(2) ** shift;
      lmult    : constant Address_Type
                  := Address_Type(2) ** (abits - shift) / wsize;
   begin
      if dir then
         if lmult /= 0 then
            return ((saddr * rmult) or (saddr / lmult)) * wsize or caddr;
         else
            return address;
         end if;
      else
         if rmult /= 0 then
            return ((saddr * lmult) or (saddr / rmult)) * wsize or caddr;
         else
            return address;
         end if;
      end if;
   end Apply;

   function To_String(mem : Shift_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(shift ");
      Append(result, "(value" & Natural'Image(mem.shift) & ")");
      if mem.bank /= null then
         Append(result, "(bank ");
         Append(result, To_String(To_String(mem.bank.all)));
         Append(result, ")");
      end if;
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   procedure Generate_Simple(mem  : in Shift_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String) is
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      other       : constant Memory_Pointer  := Get_Memory(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
      shift       : constant Integer := mem.shift;
   begin
      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);
      Line(code, name & "_inst : entity work.shift");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      SHIFT          => " & To_String(shift));
      Line(code, "   )");
      Line(code, "   port map (");
      Line(code, "      clk      => clk,");
      Line(code, "      rst      => rst,");
      Line(code, "      addr     => " & name & "_addr,");
      Line(code, "      din      => " & name & "_din,");
      Line(code, "      dout     => " & name & "_dout,");
      Line(code, "      re       => " & name & "_re,");
      Line(code, "      we       => " & name & "_we,");
      Line(code, "      mask     => " & name & "_mask,");
      Line(code, "      ready    => " & name & "_ready,");
      Line(code, "      maddr    => " & oname & "_addr,");
      Line(code, "      min      => " & oname & "_dout,");
      Line(code, "      mout     => " & oname & "_din,");
      Line(code, "      mre      => " & oname & "_re,");
      Line(code, "      mwe      => " & oname & "_we,");
      Line(code, "      mmask    => " & oname & "_mask,");
      Line(code, "      mready   => " & oname & "_ready");
      Line(code, "   );");
   end Generate_Simple;

   procedure Generate_Banked(mem  : in Shift_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String) is
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      bank        : constant Memory_Pointer  := Get_Bank(mem);
      join        : constant Join_Pointer    := Find_Join(bank);
      other       : constant Memory_Pointer  := Get_Memory(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      bname       : constant String := "m" & To_String(Get_ID(bank.all));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
      jname       : constant String := "m" & To_String(Get_ID(join.all));
      shift       : constant Integer := mem.shift;
      ishift      : constant Integer := -mem.shift;
   begin
      Generate(other.all, sigs, code);
      Generate(bank.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work.shift");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      SHIFT          => " & To_String(shift));
      Line(code, "   )");
      Line(code, "   port map (");
      Line(code, "      clk      => clk,");
      Line(code, "      rst      => rst,");
      Line(code, "      addr     => " & name & "_addr,");
      Line(code, "      din      => " & name & "_din,");
      Line(code, "      dout     => " & name & "_dout,");
      Line(code, "      re       => " & name & "_re,");
      Line(code, "      we       => " & name & "_we,");
      Line(code, "      mask     => " & name & "_mask,");
      Line(code, "      ready    => " & name & "_ready,");
      Line(code, "      maddr    => " & bname & "_addr,");
      Line(code, "      min      => " & bname & "_dout,");
      Line(code, "      mout     => " & bname & "_din,");
      Line(code, "      mre      => " & bname & "_re,");
      Line(code, "      mwe      => " & bname & "_we,");
      Line(code, "      mmask    => " & bname & "_mask,");
      Line(code, "      mready   => " & bname & "_ready");
      Line(code, "   );");

      -- Transform out of the bank.
      Line(code, jname & "_inst : entity work.shift");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      SHIFT          => " & To_String(ishift));
      Line(code, "   )");
      Line(code, "   port map (");
      Line(code, "      clk      => clk,");
      Line(code, "      rst      => rst,");
      Line(code, "      addr     => " & jname & "_addr,");
      Line(code, "      din      => " & jname & "_din,");
      Line(code, "      dout     => " & jname & "_dout,");
      Line(code, "      re       => " & jname & "_re,");
      Line(code, "      we       => " & jname & "_we,");
      Line(code, "      mask     => " & jname & "_mask,");
      Line(code, "      ready    => " & jname & "_ready,");
      Line(code, "      maddr    => " & oname & "_addr,");
      Line(code, "      min      => " & oname & "_dout,");
      Line(code, "      mout     => " & oname & "_din,");
      Line(code, "      mre      => " & oname & "_re,");
      Line(code, "      mwe      => " & oname & "_we,");
      Line(code, "      mmask    => " & oname & "_mask,");
      Line(code, "      mready   => " & oname & "_ready");
      Line(code, "   );");

   end Generate_Banked;

   function Is_Empty(mem : Shift_Type) return Boolean is
   begin
      return Is_Empty(Transform_Type(mem)) or mem.shift = 0;
   end Is_Empty;

end Memory.Transform.Shift;
