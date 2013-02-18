
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
      result.mem           := mem;
      result.line_size     := line_size;
      result.line_count    := line_count;
      result.associativity := associativity;
      result.latency       := latency;
      result.policy        := policy;
      return result;
   end Create_Cache;

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

   function Get_Last_Index(mem      : Cache_Type;
                           address  : Address_Type) return Natural is
   begin
      return Get_First_Index(mem, address) + mem.associativity - 1;
   end Get_Last_Index;

   procedure Get_Data(mem      : in out Cache_Type;
                      address  : in Address_Type;
                      is_read  : in Boolean) is

      data        : Cache_Data_Pointer;
      tag         : constant Address_Type := Get_Tag(mem, address);
      first       : constant Natural := Get_First_Index(mem, address);
      last        : constant Natural := Get_Last_Index(mem, address);
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
      -- Here we keep track of the oldest entry.
      for i in first .. last loop
         data := mem.data.Element(i);
         if tag = data.address then
            if mem.policy /= FIFO then
               data.age := 0;
            end if;
            data.dirty := data.dirty or not is_read;
            Advance(mem, cycles);
            return;
         end if;
      end loop;

      -- Not in the cache.
      -- Determine which entry to replace.
      case mem.policy is
         when LRU | FIFO =>
            age := Long_Integer'First;
            for i in first .. last loop
               data := mem.data.Element(i);
               if data.age > age then
                  age := data.age;
                  to_replace := i;
               end if;
            end loop;
         when MRU =>
            age := Long_Integer'Last;
            for i in first .. last loop
               data := mem.data.Element(i);
               if data.age < age then
                  age := data.age;
                  to_replace := i;
               end if;
            end loop;
         when Random =>
            to_replace := RNG.Random(mem.generator) mod (last - first + 1);
      end case;

      -- Not in the cache; evict the oldest entry.
      data := mem.data.Element(to_replace);
      if data.dirty then
         Start(mem.mem.all);
         Write(mem.mem.all, data.address, mem.line_size);
         Commit(mem.mem.all, cycles);
         Advance(mem, cycles);
         data.dirty := False;
      end if;

      -- Read the new entry.
      Start(mem.mem.all);
      data.address := tag;
      Read(mem.mem.all, data.address, mem.line_size);
      Commit(mem.mem.all, cycles);
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

   procedure Show_Access_Stats(mem : in Cache_Type) is
   begin
      Show_Access_Stats(mem.mem.all);
   end Show_Access_Stats;

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
      Destroy(Memory_Pointer(mem.mem));
   end Finalize;

end Memory.Cache;
