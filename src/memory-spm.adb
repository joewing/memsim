
with BRAM;
with Device; use Device;
with CACTI;

package body Memory.SPM is

   MIN_WORD_COUNT : constant := 64;

   function Create_SPM(mem       : access Memory_Type'Class;
                       size      : Natural;
                       latency   : Time_Type := 1) return SPM_Pointer is
      result : constant SPM_Pointer := new SPM_Type;
   begin
      Set_Memory(result.all, mem);
      result.size := size;
      result.latency := latency;
      return result;
   end Create_SPM;

   function Random_SPM(next      : access Memory_Type'Class;
                       generator : Distribution_Type;
                       max_cost  : Cost_Type)
                       return Memory_Pointer is
      result : SPM_Pointer := new SPM_Type;
   begin

      Set_Memory(result.all, next);
      result.size := Get_Word_Size(next.all) * MIN_WORD_COUNT;
      for i in 1 .. Random(generator) mod 16 loop
         result.size := result.size * 2;
         if Get_Cost(result.all) > max_cost then
            result.size := result.size / 2;
            exit;
         end if;
         exit when Get_Cost(result.all) >= max_cost;
      end loop;

      if Get_Cost(result.all) > max_cost then
         Set_Memory(result.all, null);
         Destroy(Memory_Pointer(result));
         return Memory_Pointer(next);
      else
         declare
            cost : constant Cost_Type := Get_Cost(result.all);
         begin
            loop
               result.size := result.size * 2;
               exit when Get_Cost(result.all) /= cost;
            end loop;
            result.size := result.size / 2;
         end;
         if Get_Device = ASIC then
            result.latency := CACTI.Get_Time(result.all);
         else
            result.latency := 2;
         end if;
         return Memory_Pointer(result);
      end if;

   end Random_SPM;

   function Clone(mem : SPM_Type) return Memory_Pointer is
      result : constant SPM_Pointer := new SPM_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out SPM_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is
      wsize    : constant Natural := Get_Word_Size(mem);
      cost     : constant Cost_Type := Get_Cost(mem);
      size     : constant Natural := mem.size;
      action   : Natural := Random(generator) mod 2;
   begin

      for i in 1 .. 2 loop
         if action = 0 then         -- Increase size.
            loop
               mem.size := mem.size * 2;
               exit when Get_Cost(mem) /= cost;
            end loop;
            exit when Get_Cost(mem) <= max_cost;
            mem.size := size;
         elsif mem.size > wsize * MIN_WORD_COUNT then -- Decrease size.
            loop
               mem.size := mem.size / 2;
               exit when Get_Cost(mem) /= cost;
            end loop;
            exit when mem.size > wsize;
            mem.size := size;
         end if;
         action := (action + 1) mod 2;
      end loop;

      -- Use up as much of this block ram as possible.
      declare
         new_cost : constant Cost_Type := Get_Cost(mem);
      begin
         loop
            mem.size := mem.size * 2;
            exit when Get_Cost(mem) /= new_cost;
         end loop;
         mem.size := mem.size / 2;
      end;

      if Get_Device = ASIC then
         mem.latency := CACTI.Get_Time(mem);
      else
         mem.latency := 2;
      end if;

   end Permute;

   procedure Read(mem      : in out SPM_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      if address >= Address_Type(mem.size) then
         Read(Container_Type(mem), address, size);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            last  : constant Address_Type := address + Address_Type(size);
            nsize : constant Positive := Positive(last - naddr);
         begin
            Read(Container_Type(mem), naddr, nsize);
         end;
      else
         Advance(mem, mem.latency);
      end if;
   end Read;

   procedure Write(mem     : in out SPM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      if address >= Address_Type(mem.size) then
         Write(Container_Type(mem), address, size);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            nsize : constant Positive := Positive(naddr - address);
         begin
            Write(Container_Type(mem), naddr, nsize);
         end;
      else
         Advance(mem, mem.latency);
      end if;
   end Write;

   function To_String(mem : SPM_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(spm ");
      Append(result, "(size" & Natural'Image(mem.size) & ")");
      Append(result, "(latency" & Time_Type'Image(mem.latency) & ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : SPM_Type) return Cost_Type is
      wsize    : constant Positive  := Get_Word_Size(mem);
      width    : constant Natural   := wsize * 8;
      depth    : constant Natural   := mem.size / wsize;
      result   : Cost_Type := 0;
   begin
      if Get_Device = ASIC then
         result := CACTI.Get_Area(mem);
      else
         result := Cost_Type(BRAM.Get_Count(width, depth));
      end if;
      result := result + Get_Cost(Container_Type(mem));
      return result;
   end Get_Cost;

   function Get_Path_Length(mem : SPM_Type) return Natural is
      asize : constant Natural := Get_Address_Bits;
   begin
      return asize + Get_Path_Length(Container_Type(mem));
   end Get_Path_Length;

   procedure Generate(mem  : in SPM_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      other       : constant Memory_Pointer  := Get_Memory(mem);
      word_bits   : constant Natural   := 8 * Get_Word_Size(mem);
      name        : constant String    := "m" & To_String(Get_ID(mem));
      oname       : constant String    := "m" & To_String(Get_ID(other.all));
      size        : constant Natural   := (8 * mem.size) / word_bits;
   begin
      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);
      Line(code, name & "_inst : entity work.spm");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH => " & To_String(word_bits) & ",");
      Line(code, "      SIZE_BITS  => " & To_String(Log2(size) - 1));
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
   end Generate;

   function Get_Size(mem : SPM_Type) return Natural is
   begin
      return mem.size;
   end Get_Size;

end Memory.SPM;
