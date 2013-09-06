
with Ada.Unchecked_Deallocation;
with Ada.Assertions; use Ada.Assertions;
with Device;         use Device;
with BRAM;
with CACTI;
with Random_Enum;

package body Memory.Cache is

   MIN_LINE_COUNT : constant := 16;

   procedure Free is
      new Ada.Unchecked_Deallocation(Cache_Data, Cache_Data_Pointer);

   function Create_Cache(mem           : access Memory_Type'Class;
                         line_count    : Positive := 1;
                         line_size     : Positive := 8;
                         associativity : Positive := 1;
                         latency       : Time_Type := 1;
                         policy        : Policy_Type := LRU;
                         write_back    : Boolean := True)
                         return Cache_Pointer is
      result : constant Cache_Pointer := new Cache_Type;
   begin
      Set_Memory(result.all, mem);
      result.line_size     := line_size;
      result.line_count    := line_count;
      result.associativity := associativity;
      result.latency       := latency;
      result.policy        := policy;
      result.write_back    := write_back;
      result.data.Set_Length(Count_Type(result.line_count));
      for i in 0 .. result.line_count - 1 loop
         result.data.Replace_Element(i, new Cache_Data);
      end loop;
      return result;
   end Create_Cache;

   function Random_Policy is new Random_Enum(Policy_Type);

   function Random_Boolean is new Random_Enum(Boolean);

   -- Set the latency based on associativity.
   procedure Update_Latency(mem : in out Cache_Type'Class) is
   begin
      if Get_Device = ASIC then
         mem.latency := CACTI.Get_Time(mem);
      else
         case mem.policy is
            when PLRU   =>
               mem.latency := 3 + Time_Type(mem.associativity) / 8;
            when others =>
               mem.latency := 3 + Time_Type(mem.associativity) / 4;
         end case;
      end if;
   end Update_Latency;

   function Random_Cache(next       : access Memory_Type'Class;
                         generator  : Distribution_Type;
                         max_cost   : Cost_Type)
                         return Memory_Pointer is
      result : Cache_Pointer := new Cache_Type;
   begin

      -- Start with everything set to the minimum.
      Set_Memory(result.all, next);
      result.line_size     := Get_Word_Size(next.all);
      result.line_count    := MIN_LINE_COUNT;
      result.associativity := 1;
      result.policy        := LRU;
      result.write_back    := True;

      -- If even the minimum cache is too costly, return null.
      if Get_Cost(result.all) > max_cost then
         Set_Memory(result.all, null);
         Destroy(Memory_Pointer(result));
         return Memory_Pointer(next);
      end if;

      -- Randomly increase parameters, reverting them if we exceed the cost.
      loop

         -- Line size.
         declare
            line_size : constant Positive := result.line_size;
         begin
            if Random_Boolean(Random(generator)) then
               result.line_size := line_size * 2;
               if Get_Cost(result.all) > max_cost then
                  result.line_size := line_size;
                  exit;
               end if;
            end if;
         end;

         -- Line count.
         declare
            line_count : constant Positive := result.line_count;
         begin
            if Random_Boolean(Random(generator)) then
               result.line_count := 2 * line_count;
               if Get_Cost(result.all) > max_cost then
                  result.line_count := line_count;
                  exit;
               end if;
            end if;
         end;

         -- Associativity.
         declare
            associativity : constant Positive := result.associativity;
         begin
            if Random_Boolean(Random(generator)) then
               result.associativity := result.associativity * 2;
               if result.associativity > result.line_count or else
                  Get_Cost(result.all) > max_cost then
                  result.associativity := associativity;
                  exit;
               end if;
            end if;
         end;

         -- Policy.
         declare
            policy : constant Policy_Type := result.policy;
         begin
            result.policy := Random_Policy(Random(generator));
            if Get_Cost(result.all) > max_cost then
               result.policy := policy;
               exit;
            end if;
         end;

         -- Type.
         declare
            write_back  : constant Boolean := result.write_back;
         begin
            result.write_back := Random_Boolean(Random(generator));
            if Get_Cost(result.all) > max_cost then
               result.write_back := write_back;
               exit;
            end if;
         end;

      end loop;

      Update_Latency(result.all);

      result.data.Set_Length(Count_Type(result.line_count));
      for i in 0 .. result.line_count - 1 loop
         result.data.Replace_Element(i, new Cache_Data);
      end loop;
      return Memory_Pointer(result);

   end Random_Cache;

   function Clone(mem : Cache_Type) return Memory_Pointer is
      result : constant Cache_Pointer := new Cache_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Cache_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is

      param_count    : constant Natural := 8;
      param          : Natural := Random(generator) mod param_count;
      line_size      : constant Positive     := mem.line_size;
      line_count     : constant Positive     := mem.line_count;
      associativity  : constant Positive     := mem.associativity;
      policy         : constant Policy_Type  := mem.policy;
      write_back     : constant Boolean      := mem.write_back;

   begin

      -- Loop until we either change a parameter or we are unable to
      -- change any parameter.
      for i in 1 .. param_count loop
         case param is
            when 0 =>      -- Increase line size
               mem.line_size := line_size * 2;
               exit when Get_Cost(mem) <= max_cost;
               mem.line_size := line_size;
            when 1 =>      -- Decrease line size
               if line_size > Get_Word_Size(Get_Memory(mem).all) then
                  mem.line_size := line_size / 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.line_size := line_size;
               end if;
            when 2 =>      -- Increase line count
               mem.line_count := line_count * 2;
               exit when Get_Cost(mem) <= max_cost;
               mem.line_count := line_count;
            when 3 =>      -- Decrease line count
               if line_count > MIN_LINE_COUNT and
                  line_count > associativity then
                  mem.line_count := line_count / 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.line_count := line_count;
               end if;
            when 4 =>      -- Increase associativity
               if associativity < line_count then
                  mem.associativity := associativity * 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.associativity := associativity;
               end if;
            when 5 =>      -- Decrease associativity
               if associativity > 1 then
                  mem.associativity := associativity / 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.associativity := associativity;
               end if;
            when 6 =>      -- Change policy
               mem.policy := Random_Policy(Random(generator));
               exit when Get_Cost(mem) <= max_cost;
               mem.policy := policy;
            when others => -- Change type
               mem.write_back := Random_Boolean(Random(generator));
               exit when Get_Cost(mem) <= max_cost;
               mem.write_back := write_back;
         end case;
         param := (param + 1) mod param_count;
      end loop;

      Update_Latency(mem);
      for i in mem.line_count .. mem.data.Last_Index loop
         declare
            dp : Cache_Data_Pointer := mem.data.Element(i);
         begin
            Free(dp);
         end;
      end loop;
      mem.data.Set_Length(Count_Type(mem.line_count));
      for i in line_count .. mem.line_count - 1 loop
         mem.data.Replace_Element(i, new Cache_Data);
      end loop;

      Assert(Get_Cost(mem) <= max_cost, "Invalid cache permutation");

   end Permute;

   procedure Get_Data(mem      : in out Cache_Type;
                      address  : in Address_Type;
                      size     : in Positive;
                      is_read  : in Boolean) is

      data        : Cache_Data_Pointer;
      mask        : constant Address_Type := Address_Type(mem.line_size - 1);
      tag         : constant Address_Type := address and not mask;
      set_count   : constant Natural := mem.line_count / mem.associativity;
      line_size   : constant Address_Type := Address_Type(mem.line_size);
      word_addr   : constant Address_Type := address / line_size;
      first       : constant Natural :=
                    Natural(word_addr mod Address_Type(set_count));
      line        : Natural;
      to_replace  : Natural := 0;
      age         : Long_Integer;
      age_sum     : Natural;

   begin

      Advance(mem, mem.latency);

      -- Update the age of all items in this set.
      age_sum := 0;
      for i in 0 .. mem.associativity - 1 loop
         line := first + i * set_count;
         data := mem.data.Element(line);
         if mem.policy = PLRU then
            age_sum := age_sum + Natural(data.age);
         else
            data.age := data.age + 1;
            Assert(data.age > 0, "invalid age");
         end if;
      end loop;

      -- First check if this address is already in the cache.
      -- Here we also keep track of the line to be replaced.
      if mem.policy = MRU then
         age := Long_Integer'Last;
      else
         age := Long_Integer'First;
      end if;
      for i in 0 .. mem.associativity - 1 loop
         line := first + i * set_count;
         data := mem.data.Element(line);
         if tag = data.address then    -- Cache hit.
            if mem.policy = PLRU then

               -- Reset ages to 0 if we marked all of them.
               if age_sum + 1 = mem.associativity then
                  for j in 0 .. mem.associativity - 1 loop
                     declare
                        temp : Cache_Data_Pointer;
                     begin
                        temp := mem.data.Element(first + j * set_count);
                        temp.age := 0;
                     end;
                  end loop;
               end if;

               -- Make this age most recently used.
               data.age := 1;

            elsif mem.policy /= FIFO then

               -- Other policies reset the age to 0.
               data.age := 0;

            end if;
            if is_read or mem.write_back then
               data.dirty := data.dirty or not is_read;
            else
               Write(Container_Type(mem), tag, mem.line_size);
            end if;
            return;
         elsif mem.policy = MRU then
            if data.age < age then
               to_replace := line;
               age := data.age;
            end if;
         elsif mem.policy = PLRU then
            if data.age = 0 then
               to_replace := line;
               age := data.age;
            end if;
         else
            if data.age > age then
               to_replace := line;
               age := data.age;
            end if;
         end if;
      end loop;

      -- If we got here, the item is not in the cache.
      if is_read or mem.write_back then

         -- Look up the line to replace.
         data := mem.data.Element(to_replace);

         -- Evict the oldest entry.
         -- On write-through caches, the dirty flag will never be set.
         if data.dirty then
            Write(Container_Type(mem), data.address, mem.line_size);
            data.dirty := False;
         end if;

         data.address   := tag;
         data.dirty     := not is_read;

         -- Update the age.
         if mem.policy = PLRU then
            if age_sum + 1 = mem.associativity then
               for j in 0 .. mem.associativity - 1 loop
                  declare
                     temp : Cache_Data_Pointer;
                  begin
                     temp := mem.data.Element(first + j * set_count);
                     temp.age := 0;
                  end;
               end loop;
            end if;
            data.age := 1;
         else
            data.age := 0;
         end if;

         -- Read the new entry.
         -- We skip this if this was a write that wrote the entire line.
         if is_read or size /= mem.line_size then
            Read(Container_Type(mem), tag, mem.line_size);
         end if;

      else

         -- A write on a write-through cache, forward the write.
         Write(Container_Type(mem), address, size);

      end if;

   end Get_Data;

   procedure Reset(mem     : in out Cache_Type;
                   context : in Natural) is
      data : Cache_Data_Pointer;
   begin
      Reset(Container_Type(mem), context);
      for i in 0 .. mem.line_count - 1 loop
         data := mem.data.Element(i);
         data.address   := Address_Type'Last;
         data.age       := 0;
         data.dirty     := False;
      end loop;
   end Reset;

   procedure Read(mem      : in out Cache_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      extra : constant Natural      := size / mem.line_size;
      abits : constant Positive     := Get_Address_Bits;
      mask  : constant Address_Type := Address_Type(2) ** abits - 1;
      temp  : Address_Type          := address;
   begin
      for i in 1 .. extra loop
         Get_Data(mem, temp, mem.line_size, True);
         temp := (temp + Address_Type(mem.line_size)) and mask;
      end loop;
      if size > extra * mem.line_size then
         Get_Data(mem, temp, size - extra * mem.line_size, True);
      end if;
   end Read;

   procedure Write(mem     : in out Cache_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      extra : constant Natural      := size / mem.line_size;
      abits : constant Positive     := Get_Address_Bits;
      mask  : constant Address_Type := Address_Type(2) ** abits - 1;
      temp  : Address_Type          := address;
   begin
      for i in 1 .. extra loop
         Get_Data(mem, temp, mem.line_size, False);
         temp := (temp + Address_Type(mem.line_size)) and mask;
      end loop;
      if size > extra * mem.line_size then
         Get_Data(mem, temp, size - extra * mem.line_size, False);
      end if;
   end Write;

   function To_String(mem : Cache_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(cache ");
      Append(result, "(line_size" & Positive'Image(mem.line_size) & ")");
      Append(result, "(line_count" & Positive'Image(mem.line_count) & ")");
      Append(result, "(associativity" &
             Positive'Image(mem.associativity) & ")");
      Append(result, "(latency" & Time_Type'Image(mem.latency) & ")");
      if mem.associativity > 1 then
         Append(result, "(policy ");
         case mem.policy is
            when LRU    => Append(result, "lru");
            when MRU    => Append(result, "mru");
            when FIFO   => Append(result, "fifo");
            when PLRU   => Append(result, "plru");
         end case;
         Append(result, ")");
      end if;
      if mem.write_back then
         Append(result, "(write_back true)");
      else
         Append(result, "(write_back false)");
      end if;
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, "))");
      return result;
   end To_String;

   function Get_Cost(mem : Cache_Type) return Cost_Type is

      -- Bits per line for storing data.
      lines       : constant Natural   := mem.line_count;
      lsize       : constant Natural   := mem.line_size;
      line_bits   : constant Natural   := lsize * 8;

      -- Bits to store a tag.
      addr_bits   : constant Positive  := Get_Address_Bits;
      wsize       : constant Positive  := Get_Word_Size(mem);
      index_bits  : constant Natural   := Log2(lines - 1);
      line_words  : constant Natural   := (lsize + wsize - 1) / wsize;
      ls_bits     : constant Natural   := Log2(line_words - 1);
      tag_bits    : constant Natural   := addr_bits - index_bits - ls_bits;

      -- Bits to store the age.
      assoc       : constant Positive  := mem.associativity;

      -- Bits used for storing valid and dirty.
      valid_bits  : constant Natural := 1;
      dirty_bits  : constant Natural := 1;

      -- Bits per way.  This is the width of the memory.
      width       : Natural := valid_bits + line_bits + tag_bits;

      result : Cost_Type;

   begin

      -- Use CACTI to determine the cost for ASICs.
      if Get_Device = ASIC then
         result := Get_Cost(Container_Type(mem));
         result := result + CACTI.Get_Area(mem);
         return result;
      end if;

      -- Determine the number of age bits.
      if assoc > 1 then
         case mem.policy is
            when PLRU =>
               width := width + 1;
            when others =>
               width := width + Log2(assoc - 1);
         end case;
      end if;

      -- If this cache is a write-back cache, we need to track a dirty
      -- bit for each cache line.
      if mem.write_back then
         width := width + dirty_bits;
      end if;

      -- The memory must be wide enough to allow access to each way.
      width := width * assoc;

      -- Given the width and depth of the cache, determine the number
      -- of BRAMs required.
      result := Cost_Type(BRAM.Get_Count(width, lines / assoc));

      -- Add the cost of the contained memory.
      result := result + Get_Cost(Container_Type(mem));

      return result;

   end Get_Cost;

   procedure Generate(mem  : in Cache_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      other    : constant Memory_Pointer := Get_Memory(mem);
      word_bits: constant Natural   := 8 * Get_Word_Size(mem);
      name     : constant String    := "m" & To_String(Get_ID(mem));
      oname    : constant String    := "m" & To_String(Get_ID(other.all));
      lsize    : constant Positive  := 8 * mem.line_size / word_bits;
      lcount   : constant Positive  := mem.line_count;
      assoc    : constant Natural   := mem.associativity;
   begin
      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);
      Line(code, name & "_inst : entity work.cache");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH      => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      Line(code, "      LINE_SIZE_BITS  => " &
            To_String(Log2(lsize - 1)) & ",");
      Line(code, "      LINE_COUNT_BITS => " &
            To_String(Log2(lcount / assoc - 1)) & ",");
      Line(code, "      ASSOC_BITS      => " &
            To_String(Log2(assoc - 1)) & ",");
      case mem.policy is
         when LRU    =>
            Line(code, "      REPLACEMENT     => 0,");
         when MRU    =>
            Line(code, "      REPLACEMENT     => 1,");
         when FIFO   =>
            Line(code, "      REPLACEMENT     => 2,");
         when PLRU   =>
            Line(code, "      REPLACEMENT     => 3,");
      end case;
      if mem.write_back then
         Line(code, "      WRITE_POLICY    => 0");
      else
         Line(code, "      WRITE_POLICY    => 1");
      end if;
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

   procedure Adjust(mem : in out Cache_Type) is
      ptr : Cache_Data_Pointer;
   begin
      Adjust(Container_Type(mem));
      for i in mem.data.First_Index .. mem.data.Last_Index loop
         ptr := new Cache_Data'(mem.data.Element(i).all);
         mem.data.Replace_Element(i, ptr);
      end loop;
   end Adjust;

   procedure Finalize(mem : in out Cache_Type) is
   begin
      Finalize(Container_Type(mem));
      for i in mem.data.First_Index .. mem.data.Last_Index loop
         declare
            ptr : Cache_Data_Pointer := mem.data.Element(i);
         begin
            Free(ptr);
         end;
      end loop;
   end Finalize;

   function Get_Line_Size(mem : Cache_Type) return Positive is
   begin
      return mem.line_size;
   end Get_Line_Size;

   function Get_Line_Count(mem : Cache_Type) return Positive is
   begin
      return mem.line_count;
   end Get_Line_Count;

   function Get_Associativity(mem : Cache_Type) return Positive is
   begin
      return mem.associativity;
   end Get_Associativity;

   function Get_Policy(mem : Cache_Type) return Policy_Type is
   begin
      return mem.policy;
   end Get_Policy;

end Memory.Cache;
