
with Device;            use Device;
with Memory.Container;  use Memory.Container;
with Memory.Join;       use Memory.Join;

package body Memory.Transform is

   procedure Process(mem      : in out Transform_Type;
                     address  : in Address_Type;
                     size     : in Address_Type;
                     dir      : in Boolean;
                     is_read  : in Boolean) is

      abits    : constant Positive := Get_Address_Bits;
      mask     : constant Address_Type := Address_Type(2) ** abits - 1;
      start    : Address_Type := address;
      trans    : Address_Type := Apply(Transform_Type'Class(mem), start, dir);
      total    : Address_Type := 0;

      incr     : Address_Type;
      nsize    : Address_Type;
      last     : Address_Type;
      temp     : Address_Type;

   begin

      trans := trans and mask;
      incr := Address_Type(Get_Alignment(Transform_Type'Class(mem)));
      while (address mod incr) /= 0 loop
         incr := incr / 2;
      end loop;

      while total < size loop

         -- Determine how big we can make the current access.
         last := trans;
         nsize := Address_Type'Min(size - total, incr);
         while total + nsize < size loop
            temp := Apply(Transform_Type'Class(mem), start + nsize, dir);
            temp := temp and mask;
            exit when ((last + nsize) and mask) /= temp;
            nsize := Address_Type'Min(size - total, nsize + incr);
         end loop;

         -- Perform the access.
         if dir and mem.bank /= null then
            if is_read then
               Read(mem.bank.all, trans, Natural(nsize));
            else
               Write(mem.bank.all, trans, Natural(nsize));
            end if;
         else
            if is_read then
               Read(Container_Type(mem), trans, Natural(nsize));
            else
               Write(Container_Type(mem), trans, Natural(nsize));
            end if;
         end if;

         total := total + nsize;
         start := start + nsize;
         trans := temp;

      end loop;

   end Process;

   procedure Set_Value(mem    : in out Transform_Type;
                       value  : in Long_Integer) is
   begin
      mem.value := value;
   end Set_Value;

   function Get_Value(mem : Transform_Type) return Long_Integer is
   begin
      return mem.value;
   end Get_Value;

   function Get_Bank(mem : Transform_Type) return Memory_Pointer is
   begin
      return Memory_Pointer(mem.bank);
   end Get_Bank;

   procedure Set_Bank(mem  : in out Transform_Type;
                      bank : access Memory_Type'Class) is
   begin
      mem.bank := bank;
   end Set_Bank;

   procedure Reset(mem     : in out Transform_Type;
                   context : in Natural) is
   begin
      Reset(Container_Type(mem), context);
      if mem.bank /= null then
         Reset(mem.bank.all, context);
      end if;
   end Reset;

   procedure Read(mem      : in out Transform_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      start : Time_Type;
   begin
      if mem.bank /= null then
         start := Get_Time(mem.bank.all);
         Process(mem, address, Address_Type(size), True, True);
         Advance(mem, Get_Time(mem.bank.all) - start);
      else
         Process(mem, address, Address_Type(size), True, True);
      end if;
   end Read;

   procedure Write(mem     : in out Transform_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      start : Time_Type;
   begin
      if mem.bank /= null then
         start := Get_Time(mem.bank.all);
         Process(mem, address, Address_Type(size), True, False);
         Advance(mem, Get_Time(mem.bank.all) - start);
      else
         Process(mem, address, Address_Type(size), True, False);
      end if;
   end Write;

   procedure Forward_Read(mem       : in out Transform_Type;
                          source    : in Natural;
                          address   : in Address_Type;
                          size      : in Positive) is
   begin
      Process(mem, address, Address_Type(size), False, True);
   end Forward_Read;

   procedure Forward_Write(mem      : in out Transform_Type;
                           source   : in Natural;
                           address  : in Address_Type;
                           size     : in Positive) is
   begin
      Process(mem, address, Address_Type(size), False, False);
   end Forward_Write;

   procedure Forward_Idle(mem    : in out Transform_Type;
                          source : in Natural;
                          cycles : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
   end Forward_Idle;

   function Forward_Get_Time(mem : Transform_Type) return Time_Type is
   begin
      return Get_Time(Container_Type(mem));
   end Forward_Get_Time;

   function Get_Join_Length(mem : Transform_Type) return Natural is
   begin
      return Get_Transform_Length(Transform_Type'Class(mem));
   end Get_Join_Length;

   function Get_Cost(mem : Transform_Type) return Cost_Type is
      result : Cost_Type := Get_Cost(Container_Type(mem));
   begin
      if mem.bank /= null then
         result := result + Get_Cost(mem.bank.all);
      end if;
      return result;
   end Get_Cost;

   function Is_Empty(mem : Transform_Type) return Boolean is
   begin
      return False;
   end Is_Empty;

   function Get_Alignment(mem : Transform_Type) return Positive is
   begin
      return 1;
   end Get_Alignment;

   function To_String(mem : Transform_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(" & Get_Name(Transform_Type'Class(mem)) & " ");
      if mem.value /= 0 then
         Append(result, "(value " & To_String(mem.value) & ")");
      end if;
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

   procedure Generate_Simple(mem  : in Transform_Type;
                             sigs : in out Unbounded_String;
                             code : in out Unbounded_String) is
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      other       : constant Memory_Pointer  := Get_Memory(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
      tname       : constant String := Get_Name(Transform_Type'Class(mem));
      value       : constant Long_Integer := mem.value;
   begin

      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work." & tname);
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

   procedure Generate_Banked(mem  : in Transform_Type;
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
      tname       : constant String := Get_Name(Transform_Type'Class(mem));
      value       : constant Long_Integer := mem.value;
   begin

      Generate(other.all, sigs, code);
      Generate(bank.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);

      -- Transform into the bank.
      Line(code, name & "_inst : entity work." & tname);
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
      Line(code, jname & "_inst : entity work." & tname);
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH     => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH     => " & To_String(word_bits) & ",");
      Line(code, "      VALUE          => " & To_String(-value));
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

   procedure Generate(mem  : in Transform_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
   begin
      if mem.bank /= null then
         Generate_Banked(mem, sigs, code);
      else
         Generate_Simple(mem, sigs, code);
      end if;
   end Generate;

   function Get_Path_Length(mem : Transform_Type) return Natural is
      len : Natural := Get_Transform_Length(Transform_Type'Class(mem));
   begin
      if mem.bank /= null then
         len := len + Get_Path_Length(mem.bank.all);
      else
         len := len + Get_Path_Length(Container_Type(mem));
      end if;
      return len;
   end Get_Path_Length;

   procedure Adjust(mem : in out Transform_Type) is
      jp    : Join_Pointer;
      cp    : Container_Pointer;
      ptr   : Memory_Pointer;
   begin
      Adjust(Container_Type(mem));
      if mem.bank /= null then
         mem.bank := Clone(mem.bank.all);
         ptr := Memory_Pointer(mem.bank);
         loop
            if ptr.all in Join_Type'Class then
               jp := Join_Pointer(ptr);
               Set_Parent(jp.all, mem'Unchecked_Access);
               exit;
            else
               cp := Container_Pointer(ptr);
               ptr := Get_Memory(cp.all);
            end if;
         end loop;
      end if;
   end Adjust;

   procedure Finalize(mem : in out Transform_Type) is
   begin
      Destroy(Memory_Pointer(mem.bank));
      Finalize(Container_Type(mem));
   end Finalize;

end Memory.Transform;
