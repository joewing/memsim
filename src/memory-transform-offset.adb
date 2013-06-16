
with Memory.Join; use Memory.Join;

package body Memory.Transform.Offset is

   function Create_Offset return Offset_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      result.name := To_Unbounded_String("offset");
      return result;
   end Create_Offset;

   function Random_Offset(next      : access Memory_Type'Class;
                          generator : RNG.Generator;
                          max_cost  : Cost_Type) return Memory_Pointer is
      result   : constant Offset_Pointer := Create_Offset;
      wsize    : constant Natural := Get_Word_Size(next.all);
      base     : Integer;
   begin
      Set_Memory(result.all, next);

      if (RNG.Random(generator) mod 2) = 0 then
         -- Byte offset.
         base := RNG.Random(generator) mod wsize;
      else
         -- Word offset.
         base := 2 ** (RNG.Random(generator) mod 16);
      end if;

      if (RNG.Random(generator) mod 2) = 0 then
         -- Negative offset.
         result.value := -(wsize * base);
      else
         -- Positive offset.
         result.value := wsize * base;
      end if;

      return Memory_Pointer(result);
   end Random_Offset;

   function Clone(mem : Offset_Type) return Memory_Pointer is
      result : constant Offset_Pointer := new Offset_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Offset_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
      wsize : constant Natural := Get_Word_Size(mem);
   begin
      case RNG.Random(generator) mod 4 is
         when 0      => -- Add word offset
            mem.value := mem.value + wsize;
         when 1      => -- Subtract word offset.
            mem.value := mem.value - wsize;
         when 2      => -- Add byte offset
            mem.value := mem.value + 1;
         when others => -- Subtract byte offset
            mem.value := mem.value - 1;
      end case;
   end Permute;

   function Apply(mem      : Offset_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      offset : Address_Type;
   begin
      if mem.value < 0 then
         offset := 0 - Address_Type(-mem.value);
      else
         offset := Address_Type(mem.value);
      end if;
      if dir then
         return address + offset;
      else
         return address - offset;
      end if;
   end Apply;

   function To_String(mem : Offset_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(offset ");
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

   procedure Generate_Simple(mem  : in Offset_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String) is
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      other       : constant Memory_Pointer  := Get_Memory(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
      offset      : constant Integer := mem.value;
   begin

      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work.offset");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      OFFSET         => " & To_String(offset));
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

   procedure Generate_Banked(mem  : in Offset_Type;
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
      offset      : constant Integer := mem.value;
   begin

      Generate(other.all, sigs, code);
      Generate(bank.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work.offset");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      OFFSET         => " & To_String(offset));
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
      Line(code, jname & "_inst : entity work.offset");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      OFFSET         => " & To_String(-offset));
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

   function Is_Empty(mem : Offset_Type) return Boolean is
   begin
      return Is_Empty(Transform_Type(mem)) or mem.value = 0;
   end Is_Empty;

   function Get_Alignment(mem : Offset_Type) return Positive is
      alignment   : Positive := 1;
   begin
      while (mem.value mod alignment) = 0 and alignment < 2 ** 16 loop
         alignment := alignment * 2;
      end loop;
      return alignment;
   end Get_Alignment;

end Memory.Transform.Offset;
