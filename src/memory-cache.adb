
with Ada.Unchecked_Deallocation;

package body Memory.Cache is

   function Create_Cache(mem           : access Memory_Type'Class;
                         line_count    : Positive := 1;
                         line_size     : Positive := 8;
                         associativity : Positive := 1;
                         latency       : Time_Type := 1;
                         policy        : Policy_Type := LRU)
                         return Cache_Pointer is
      result : constant Cache_Pointer := new Cache_Type;
   begin
      Set_Memory(result.all, mem);
      result.line_size     := line_size;
      result.line_count    := line_count;
      result.associativity := associativity;
      result.latency       := latency;
      result.policy        := policy;
      return result;
   end Create_Cache;

   function Random_Boolean(generator : RNG.Generator) return Boolean is
   begin
      return (RNG.Random(generator) mod 2) = 1;
   end Random_Boolean;

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
      result.policy        := Random;

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
            if Random_Boolean(generator) then
               result.line_size := line_size * 2;
               if Get_Cost(result.all) > max_cost then
                  result.line_size := line_size;
               end if;
            end if;
         end;

         -- Line count.
         declare
            line_count : constant Positive := result.line_count;
         begin
            if Random_Boolean(generator) then
               result.line_count := 2 * line_count;
               if Get_Cost(result.all) > max_cost then
                  result.line_count := line_count;
               end if;
            end if;
         end;

         -- Associativity.
         declare
            associativity : constant Positive := result.associativity;
         begin
            if Random_Boolean(generator) then
               result.associativity := result.associativity * 2;
               if result.associativity > result.line_count or else
                  Get_Cost(result.all) > max_cost then
                  result.associativity := associativity;
               end if;
            end if;
         end;

         -- Policy.
         declare
            policy : constant Policy_Type := result.policy;
         begin
            case RNG.Random(generator) mod 4 is
               when 0      => result.policy := LRU;
               when 1      => result.policy := MRU;
               when 2      => result.policy := FIFO;
               when others => result.policy := Random;
            end case;
            if Get_Cost(result.all) > max_cost then
               result.policy := policy;
            end if;
         end;

         -- 1 in 8 chance of exiting after adjusting parameters.
         exit when (RNG.Random(generator) mod 16) = 0;

      end loop;

      -- No point in creating a worthless cache.
      if result.line_size = 1 and result.line_count = 1 then
         Destroy(Memory_Pointer(result));
         return null;
      else
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

      param          : Natural := RNG.Random(generator) mod 8;
      line_size      : constant Positive := mem.line_size;
      line_count     : constant Positive := mem.line_count;
      associativity  : constant Positive := mem.associativity;
      policy         : constant Policy_Type := mem.policy;

   begin

      -- Loop until we either change a parameter or we are unable to
      -- change any parameter.
      for i in 1 .. 8 loop
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
               if line_count > 1 then
                  mem.line_count := line_count / 2;
                  if mem.associativity > mem.line_count then
                     mem.associativity := mem.line_count;
                  end if;
                  exit;
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
                  exit;
               end if;
            when 6 =>      -- Increase policy
               if policy /= Policy_Type'Last then
                  mem.policy := Policy_Type'Succ(policy);
                  exit when Get_Cost(mem) <= max_cost;
                  mem.policy := policy;
               end if;
            when others => -- Decrease policy
               if policy /= Policy_Type'First then
                  mem.policy := Policy_Type'Pred(policy);
                  exit when Get_Cost(mem) <= max_cost;
                  mem.policy := policy;
               end if;
         end case;
         param := (param + 1) mod 8;
      end loop;

   end Permute;

   function Get_Tag(mem       : Cache_Type;
                    address   : Address_Type) return Address_Type is
      mask : constant Address_Type := not Address_Type(mem.line_size - 1);
   begin
      return address and mask;
   end Get_Tag;

   function Get_Index(mem     : Cache_Type;
                      address : Address_Type) return Natural is
      set_size    : constant Natural := mem.line_size * mem.associativity;
      set_count   : constant Natural := mem.line_count / mem.associativity;
      base        : constant Address_Type := address / Address_Type(set_size);
   begin
      return Natural((Long_Integer(base) mod Long_Integer(set_count)))
               * set_size;
   end Get_Index;

   function Get_First_Index(mem     : Cache_Type;
                            address : Address_Type) return Natural is
   begin
      return Get_Index(mem, address);
   end Get_First_Index;

   procedure Get_Data(mem      : in out Cache_Type;
                      address  : in Address_Type;
                      is_read  : in Boolean) is

      data        : Cache_Data_Pointer;
      tag         : constant Address_Type := Get_Tag(mem, address);
      first       : constant Natural := Get_First_Index(mem, address);
      last        : constant Natural := first + mem.associativity - 1;
      to_replace  : Natural := 0;
      age         : Long_Integer;
      cycles      : Time_Type := mem.latency;

   begin

      -- Make sure the vector is big enough.
      for i in Natural(Cache_Vectors.Length(mem.data)) .. last loop
         Cache_Vectors.Append(mem.data, new Cache_Data);
      end loop;

      -- Update the age of all items in this set.
      for i in first .. last loop
         data := Cache_Vectors.Element(mem.data, i);
         data.age := data.age + 1;
      end loop;

      -- First check if this address is already in the cache.
      -- Here we also keep track of the line to be replaced.
      if mem.policy = MRU then
         age := Long_Integer'Last;
      else
         age := Long_Integer'First;
      end if;
      for i in first .. last loop
         data := mem.data.Element(i);
         if tag = data.address then
            if mem.policy /= FIFO then
               data.age := 0;
            end if;
            data.dirty := data.dirty or not is_read;
            Advance(mem, cycles);
            return;
         elsif mem.policy = MRU then
            if data.age < age then
               to_replace := i;
               age := data.age;
            end if;
         else
            if data.age > age then
               to_replace := i;
               age := data.age;
            end if;
         end if;
      end loop;

      if mem.policy = Random then
         to_replace := RNG.Random(mem.generator.all) mod (last - first + 1);
      end if;

      -- Not in the cache; evict the oldest entry.
      data := mem.data.Element(to_replace);
      if data.dirty then
         Forward_Start(mem);
         Forward_Write(mem, data.address, mem.line_size);
         Forward_Commit(mem, cycles);
         Advance(mem, cycles);
         data.dirty := False;
      end if;

      -- Read the new entry.
      Forward_Start(mem);
      data.address := tag;
      Forward_Read(mem, data.address, mem.line_size);
      Forward_Commit(mem, cycles);
      Advance(mem, cycles);

      -- Mark the data as new and return.
      data.age := 0;
      data.dirty := not is_read;

   end Get_Data;

   procedure Read(mem      : in out Cache_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      extra : constant Natural := size / mem.line_size;
   begin
      for i in 0 .. extra loop
         Get_Data(mem, address + Address_Type(i * mem.line_size), True);
      end loop;
   end Read;

   procedure Write(mem     : in out Cache_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      extra : constant Natural := size / mem.line_size;
   begin
      for i in 0 .. extra loop
         Get_Data(mem, address + Address_Type(i * mem.line_size), False);
      end loop;
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
      Append(result, "(policy ");
      case mem.policy is
         when LRU    => Append(result, "lru");
         when MRU    => Append(result, "mru");
         when FIFO   => Append(result, "fifo");
         when Random => Append(result, "random");
      end case;
      Append(result, ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, "))");
      return result;
   end To_String;

   function Get_Cost(mem : Cache_Type) return Cost_Type is

      -- Number of transistors to store the data.
      lines    : constant Cost_Type := Cost_Type(mem.line_count);
      lsize    : constant Cost_Type := Cost_Type(mem.line_size);
      assoc    : constant Cost_Type := Cost_Type(mem.associativity);
      cells    : constant Cost_Type := 6 * lines * lsize * 8;

      -- Number of transistors needed for the address decoder.
      decoder  : constant Cost_Type := 2 * (Address_Type'Size +
                                            Cost_Type(Log2(mem.line_count)));

      -- Number of transistors needed to store tags.
      tag_size : constant Cost_Type
                  := Cost_Type(Address_Type'Size / mem.line_size +
                               mem.associativity);
      tags     : constant Cost_Type := 6 * tag_size * lines;

      -- Number of transistors needed to store age data.
      age      : constant Cost_Type := 6 * (assoc - 1);

      -- Number of transistors needed for comparators.
      compare  : constant Cost_Type := 8 * (assoc - 1) * tag_size;

      -- Number of transistors for the cache with no policy.
      base     : constant Cost_Type := cells + decoder + tags + compare;

      -- Cost of the contained memory.
      con      : constant Cost_Type := Get_Cost(Container_Type(mem));

   begin
      case mem.policy is
         when LRU    =>
            return base + age + con;
         when MRU    =>
            return base + age + con;
         when FIFO   =>
            return base + age + con;
         when Random =>
            return base + con;
      end case;
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
