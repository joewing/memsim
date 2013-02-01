
package body Memory.Cache is

   function Create_Cache(mem           : access Memory_Type'class;
                         line_count    : Natural := 1;
                         line_size     : Natural := 1;
                         associativity : Natural := 1;
                         latency       : Time_Type := 1) return Cache_Pointer is
      result : constant Cache_Pointer := new Cache_Type;
   begin
      result.mem           := mem;
      result.line_size     := line_size;
      result.line_count    := line_count;
      result.associativity := associativity;
      result.latency       := latency;
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
      return (Natural(base) mod set_count) * set_size;
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
      oldest      : Natural := 0;
      oldest_age  : Natural := 0;
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
            data.age := 0;
            data.dirty := data.dirty or not is_read;
            Advance(mem, cycles);
            return;
         elsif data.age > oldest_age then
            oldest_age := data.age;
            oldest := i;
         end if;
      end loop;

      -- Not in the cache; evict the oldest entry.
      data := mem.data.Element(oldest);
      if data.dirty then
         Start(mem.mem.all);
         for i in 0 .. mem.line_size - 1 loop
            Write(mem.mem.all, data.address + Address_Type(i));
         end loop;
         Commit(mem.mem.all, cycles);
         data.dirty := False;
      end if;

      -- Read the new entry.
      Start(mem.mem.all);
      data.address := tag;
      for i in 0 .. mem.line_size - 1 loop
         declare
            full_address : constant Address_Type
                         := data.address + Address_Type(i);
         begin
            if is_read or full_address /= address then
               Read(mem.mem.all, data.address + Address_Type(i));
            end if;
         end;
      end loop;
      Commit(mem.mem.all, cycles);

      -- Mark the data as new and return.
      data.age := 0;
      data.dirty := not is_read;
      Advance(mem, cycles);

   end Get_Data;

   procedure Read(mem      : in out Cache_Type;
                  address  : in Address_Type) is
   begin
      Get_Data(mem, address, True);
   end Read;

   procedure Write(mem     : in out Cache_Type;
                   address : in Address_Type) is
   begin
      Get_Data(mem, address, False);
   end Write;

end Memory.Cache;
