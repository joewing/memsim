
with Memory.Join; use Memory.Join;

package body Memory.Transform.EOR is

   function Create_EOR return EOR_Pointer is
      result : constant EOR_Pointer := new EOR_Type;
   begin
      result.name := To_Unbounded_String("eor");
      return result;
   end Create_EOR;

   function Random_EOR(next      : access Memory_Type'Class;
                       generator : RNG.Generator;
                       max_cost  : Cost_Type) return Memory_Pointer is
      result   : constant EOR_Pointer := Create_EOR;
      abits    : constant Positive := Address_Type'Size;
      bit      : constant Positive := 2 ** (RNG.Random(generator) mod abits);
   begin
      Set_Memory(result.all, next);
      result.value := bit;
      return Memory_Pointer(result);
   end Random_EOR;

   function Clone(mem : EOR_Type) return Memory_Pointer is
      result : constant EOR_Pointer := new EOR_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   function "xor"(a: Integer; b: Integer) return Integer is
      type MT is mod 2 ** Integer'Size;
      tempa : constant MT := MT'Mod(a);
      tempb : constant MT := MT'Mod(b);
   begin
      return Integer(tempa xor tempb);
   end "xor";

   procedure Permute(mem         : in out EOR_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
      abits : constant Positive := Address_Type'Size;
      bit   : constant Positive := 2 ** (RNG.Random(generator) mod abits);
   begin
      mem.value := mem.value xor bit;
   end Permute;

   function To_String(mem : EOR_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(xor ");
      Append(result, "(value" & Integer'Image(mem.value) & ")");
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

   procedure Generate_Simple(mem  : in EOR_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String) is

      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      other       : constant Memory_Pointer  := Get_Memory(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
      value       : constant Integer := mem.value;

   begin

      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work.eor");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      VALUE          => " & To_String(value));
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

   procedure Generate_Banked(mem  : in EOR_Type;
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
      value       : constant Integer := mem.value;
   begin

      Generate(other.all, sigs, code);
      Generate(bank.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work.eor");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      VALUE          => " & To_String(value));
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
      Line(code, jname & "_inst : entity work.eor");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      VALUE          => " & To_String(value));
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

   function Is_Empty(mem : EOR_Type) return Boolean is
   begin
      return mem.value = 0;
   end Is_Empty;

   function Apply(mem      : EOR_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
   begin
      return address xor Address_Type'Mod(mem.value);
   end Apply;

   function Get_Alignment(mem : EOR_Type) return Positive is
   begin
      for i in 0 .. 16 loop
         if (Address_Type'Mod(mem.value) and (2 ** i)) /= 0 then
            return Positive(2 ** i);
         end if;
      end loop;
      return 2 ** 16;
   end Get_Alignment;

end Memory.Transform.EOR;
