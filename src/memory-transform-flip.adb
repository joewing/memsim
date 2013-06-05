
with Memory.Join; use Memory.Join;
with Util;        use Util;

package body Memory.Transform.Flip is

   function Create_Flip return Flip_Pointer is
      result : constant Flip_Pointer := new Flip_Type;
   begin
      return result;
   end Create_Flip;

   function Random_Flip(next        : access Memory_Type'Class;
                        generator   : RNG.Generator;
                        max_cost    : Cost_Type) return Memory_Pointer is
      result : constant Flip_Pointer := new Flip_Type;
   begin
      Set_Memory(result.all, next);
      return Memory_Pointer(result);
   end Random_Flip;

   function Clone(mem : Flip_Type) return Memory_Pointer is
      result : constant Flip_Pointer := new Flip_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Flip_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      null;
   end Permute;

   function Apply(mem      : Flip_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      addr_bits   : constant Natural := Address_Type'Size; -- FIXME
      word_bytes  : constant Natural := Get_Word_Size(mem);
      src_mask    : Address_Type;
      dest_mask   : Address_Type;
      result      : Address_Type := address mod Address_Type(word_bytes);
   begin
      src_mask    := Address_Type(2) ** (addr_bits - 1);
      dest_mask   := Address_Type(word_bytes);
      for i in 0 .. addr_bits - Log2(word_bytes) loop
         if (address and src_mask) /= 0 then
            result := result or dest_mask;
         end if;
         src_mask  := src_mask  / 2;
         dest_mask := dest_mask * 2;
      end loop;
      return result;
   end Apply;

   function To_String(mem : Flip_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(flip ");
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

   procedure Generate_Simple(mem  : in Flip_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String) is
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      other       : constant Memory_Pointer  := Get_Memory(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
   begin

      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      Line(code, name & "_inst : entity work.flip");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits));
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

   procedure Generate_Banked(mem  : in Flip_Type;
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
   begin

      Generate(other.all, sigs, code);
      Generate(bank.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work.flip");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits));
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
      Line(code, jname & "_inst : entity work.flip");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits));
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

   function Get_Alignment(mem : Flip_Type) return Positive is
   begin
      return Get_Word_Size(mem);
   end Get_Alignment;

end Memory.Transform.Flip;
