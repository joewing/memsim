
with Ada.Unchecked_Deallocation;
with Ada.Assertions; use Ada.Assertions;
with Random_Enum;

package body Memory.Cache is

   function Create_Cache(mem           : access Memory_Type'Class;
                         line_count    : Positive := 1;
                         line_size     : Positive := 8;
                         associativity : Positive := 1;
                         latency       : Time_Type := 1;
                         policy        : Policy_Type := LRU;
                         exclusive     : Boolean := False;
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
      result.exclusive     := exclusive;
      result.write_back    := write_back;
      result.data.Set_Length(Count_Type(result.line_count));
      for i in 0 .. result.line_count - 1 loop
         result.data.Replace_Element(i, new Cache_Data);
      end loop;
      return result;
   end Create_Cache;

   function Random_Policy is new Random_Enum(Policy_Type);

   function Random_Boolean is new Random_Enum(Boolean);

   function Random_Cache(generator  : RNG.Generator;
                         max_cost   : Cost_Type)
                         return Memory_Pointer is
      result : Cache_Pointer := new Cache_Type;
   begin

      -- Start with everything set to the minimum.
      result.line_size     := 1;
      result.line_count    := 1;
      result.associativity := 1;
      result.latency       := 1;
      result.policy        := LRU;
      result.exclusive     := False;
      result.write_back    := True;

      -- If even the minimum cache is too costly, return nulll.
      if Get_Cost(result.all) > max_cost then
         Destroy(Memory_Pointer(result));
         return null;
      end if;

      -- Randomly increase parameters, reverting them if we exceed the cost.
      loop

         -- Line size.
         declare
            line_size : constant Positive := result.line_size;
         begin
            if Random_Boolean(RNG.Random(generator)) then
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
            if Random_Boolean(RNG.Random(generator)) then
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
            if Random_Boolean(RNG.Random(generator)) then
               result.associativity := result.associativity * 2;
               if result.associativity > result.line_count or else
                  Get_Cost(result.all) > max_cost then
                  result.associativity := associativity;
                  exit;
               end if;
            end if;
         end;

         -- Latency.
         declare
            latency : constant Time_Type := result.latency;
         begin
            if Random_Boolean(RNG.Random(generator)) then
               result.latency := result.latency * 2;
               if result.latency > Time_Type(result.associativity) or else
                  Get_Cost(result.all) > max_cost then
                  result.latency := latency;
                  exit;
               end if;
            end if;
         end;

         -- Policy.
         declare
            policy : constant Policy_Type := result.policy;
         begin
            result.policy := Random_Policy(RNG.Random(generator));
            if Get_Cost(result.all) > max_cost then
               result.policy := policy;
               exit;
            end if;
         end;

         -- Type.
         declare
            exclusive   : constant Boolean := result.exclusive;
            write_back  : constant Boolean := result.write_back;
         begin
            result.exclusive  := Random_Boolean(RNG.Random(generator));
            result.write_back := Random_Boolean(RNG.Random(generator));
            if Get_Cost(result.all) > max_cost then
               result.exclusive := exclusive;
               result.write_back := write_back;
               exit;
            end if;
         end;

      end loop;

      -- No point in creating a worthless cache.
      Assert(Get_Cost(result.all) <= max_cost, "Invalid cache");
      if result.line_size = 1 and result.line_count = 1 then
         Destroy(Memory_Pointer(result));
         return null;
      else
         result.data.Set_Length(Count_Type(result.line_count));
         for i in 0 .. result.line_count - 1 loop
            result.data.Replace_Element(i, new Cache_Data);
         end loop;
         return Memory_Pointer(result);
      end if;

   end Random_Cache;

   function Clone(mem : Cache_Type) return Memory_Pointer is
      result : constant Cache_Pointer := new Cache_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Cache_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is

      param          : Natural := RNG.Random(generator) mod 10;
      line_size      : constant Positive     := mem.line_size;
      line_count     : constant Positive     := mem.line_count;
      associativity  : constant Positive     := mem.associativity;
      latency        : constant Time_Type    := mem.latency;
      policy         : constant Policy_Type  := mem.policy;
      exclusive      : constant Boolean      := mem.exclusive;
      write_back     : constant Boolean      := mem.write_back;

   begin

      -- Loop until we either change a parameter or we are unable to
      -- change any parameter.
      for i in 1 .. 10 loop
         case param is
            when 0 =>      -- Increase line size
               mem.line_size := line_size * 2;
               exit when Get_Cost(mem) <= max_cost;
               mem.line_size := line_size;
            when 1 =>      -- Decrease line size
               if line_size > 1 then
                  mem.line_size := line_size / 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.line_size := line_size;
               end if;
            when 2 =>      -- Increase line count
               mem.line_count := line_count * 2;
               exit when Get_Cost(mem) <= max_cost;
               mem.line_count := line_count;
            when 3 =>      -- Decrease line count
               if line_count > 1 and line_count > associativity then
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
               if associativity > 1 and associativity > Positive(latency) then
                  mem.associativity := associativity / 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.associativity := associativity;
               end if;
            when 6 =>      -- Increase latency.
               if latency < Time_Type(associativity) then
                  mem.latency := latency * 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.latency := latency;
               end if;
            when 7 =>      -- Decrease latency.
               if latency > 1 then
                  mem.latency := latency / 2;
                  exit when Get_Cost(mem) <= max_cost;
                  mem.latency := latency;
               end if;
            when 8 =>      -- Change policy
               mem.policy := Random_Policy(RNG.Random(generator));
               exit when Get_Cost(mem) <= max_cost;
               mem.policy := policy;
            when others => -- Change type
               mem.exclusive  := Random_Boolean(RNG.Random(generator));
               mem.write_back := Random_Boolean(RNG.Random(generator));
               exit when Get_Cost(mem) <= max_cost;
               mem.exclusive := exclusive;
               mem.write_back := write_back;
         end case;
         param := (param + 1) mod 10;
      end loop;

      mem.data.Set_Length(Count_Type(mem.line_count));
      for i in line_count .. mem.line_count - 1 loop
         mem.data.Replace_Element(i, new Cache_Data);
      end loop;

      Assert(Get_Cost(mem) <= max_cost, "Invalid cache permutation");

   end Permute;

   function Get_Tag(mem       : Cache_Type;
                    address   : Address_Type) return Address_Type is
      mask : constant Address_Type := not Address_Type(mem.line_size - 1);
   begin
      return address and mask;
   end Get_Tag;

   function Get_Index(mem     : Cache_Type;
                      address : Address_Type) return Natural is
      line_size   : constant Address_Type := Address_Type(mem.line_size);
      line_count  : constant Address_Type := Address_Type(mem.line_count);
      assoc       : constant Address_Type := Address_Type(mem.associativity);
      set_count   : constant Address_Type := line_count / assoc;
      base        : constant Address_Type := address / line_size;
   begin
      return Natural(base mod set_count);
   end Get_Index;

   procedure Get_Data(mem      : in out Cache_Type;
                      address  : in Address_Type;
                      size     : in Positive;
                      is_read  : in Boolean) is

      data        : Cache_Data_Pointer;
      tag         : constant Address_Type := Get_Tag(mem, address);
      first       : constant Natural := Get_Index(mem, address);
      line        : Natural;
      to_replace  : Natural := 0;
      age         : Long_Integer;

   begin

      -- Update the age of all items in this set.
      for i in 0 .. mem.associativity - 1 loop
         line := first + i * mem.line_count / mem.associativity;
         data := mem.data.Element(line);
         data.age := data.age + 1;
      end loop;

      -- First check if this address is already in the cache.
      -- Here we also keep track of the line to be replaced.
      if mem.policy = MRU then
         age := Long_Integer'Last;
      else
         age := Long_Integer'First;
      end if;
      for i in 0 .. mem.associativity - 1 loop
         line := first + i * mem.line_count / mem.associativity;
         data := mem.data.Element(line);
         if tag = data.address then    -- Cache hit.
            if mem.policy /= FIFO then
               data.age := 0;
            end if;
            if is_read or mem.write_back then
               Advance(mem, mem.latency);
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
         else
            if data.age > age then
               to_replace := line;
               age := data.age;
            end if;
         end if;
      end loop;

      -- If we got here, the item is not in the cache.
      -- If this is a read on an exclusive cache, we just forward the
      -- read the return without caching, otherwise we need to evict the
      -- oldest entry.
      if mem.exclusive and is_read then

         Read(Container_Type(mem), tag, mem.line_size);

      else

         -- Evict the oldest entry.
         -- On write-through caches, the dirty flag will never be set.
         data := mem.data.Element(to_replace);
         if data.dirty then
            Write(Container_Type(mem), data.address, mem.line_size);
            data.dirty := False;
         end if;

         -- Read the new entry.
         -- We skip this if this was a write that wrote the entire line.
         if is_read or size /= mem.line_size then
            data.address := tag;
            Read(Container_Type(mem), data.address, mem.line_size);
            data.age := 0;
            data.dirty := not is_read;
         elsif not is_read then
            Advance(mem, mem.latency);
         end if;

      end if;

   end Get_Data;

   procedure Reset(mem : in out Cache_Type) is
      data : Cache_Data_Pointer;
   begin
      Reset(Container_Type(mem));
      RNG.Reset(mem.generator.all);
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
      extra : constant Natural := size / mem.line_size;
   begin
      for i in 0 .. extra - 1 loop
         Get_Data(mem, address + Address_Type(i * mem.line_size),
                  mem.line_size, True);
      end loop;
      if size > extra * mem.line_size then
         Get_Data(mem, address + Address_Type(extra * mem.line_size),
                  size - extra * mem.line_size, True);
      end if;
   end Read;

   procedure Write(mem     : in out Cache_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      extra : constant Natural := size / mem.line_size;
   begin
      for i in 0 .. extra - 1 loop
         Get_Data(mem, address + Address_Type(i * mem.line_size),
                  mem.line_size, False);
      end loop;
      if size > extra * mem.line_size then
         Get_Data(mem, address + Address_Type(extra * mem.line_size),
                  size - extra * mem.line_size, False);
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
         end case;
         Append(result, ")");
      end if;
      if mem.exclusive then
         Append(result, "(exclusive true)");
      else
         Append(result,  "(exclusive false)");
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

      lines    : constant Cost_Type := Cost_Type(mem.line_count);
      lsize    : constant Cost_Type := Cost_Type(mem.line_size) * 8;
      assoc    : constant Cost_Type := Cost_Type(mem.associativity);

      -- Bits to store a tag.
      tag_bits : constant Cost_Type := Cost_Type(Address_Type'Size /
                                                 mem.line_size +
                                                 mem.associativity);

      -- Bits to store the age.
      age_bits : constant Cost_Type := assoc - 1;

      -- Minimum number of banks needed.
      min_banks : constant Cost_Type := Cost_Type(mem.associativity) /
                                        Cost_Type(mem.latency);

      -- Bits per line.
      line_bits   : Cost_Type := lsize + tag_bits;

      result : Cost_Type;

   begin

      -- Determine how many bits are in each line.
      line_bits := lsize + tag_bits + age_bits;

      -- If this cache is a write-back cache, we need to track a dirty
      -- bit for each cache line.
      if mem.write_back then
         line_bits := line_bits + 1;
      end if;

      -- Divide the line bits amoung the banks.
      line_bits := (line_bits + min_banks - 1) / min_banks;

      -- Now we need to store (line_bits * lines) bits of data in each bank.
      -- If this is too much for a BRAM, we need to split further.
      Assert(min_banks > 0);
      result := min_banks * ((line_bits * lines + BRAM_SIZE - 1) / BRAM_SIZE);

      -- Add the cost of the contained memory.
      result := result + Get_Cost(Container_Type(mem));

      return result;

   end Get_Cost;

   procedure Adjust(mem : in out Cache_Type) is
      ptr : Cache_Data_Pointer;
   begin
      for i in mem.data.First_Index .. mem.data.Last_Index loop
         ptr := new Cache_Data'(mem.data.Element(i).all);
         mem.data.Replace_Element(i, ptr);
      end loop;
      mem.generator := new RNG.Generator;
   end Adjust;

   procedure Free is
      new Ada.Unchecked_Deallocation(Cache_Data, Cache_Data_Pointer);

   procedure Finalize(mem : in out Cache_Type) is
   begin
      for i in mem.data.First_Index .. mem.data.Last_Index loop
         declare
            ptr : Cache_Data_Pointer := mem.data.Element(i);
         begin
            Free(ptr);
         end;
      end loop;
      Destroy(mem.generator);
   end Finalize;

end Memory.Cache;
