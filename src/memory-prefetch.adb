
package body Memory.Prefetch is

   function Create_Prefetch(mem        : access Memory_Type'Class;
                            stride     : Address_Type := 1;
                            multiplier : Address_Type := 1)
                            return Prefetch_Pointer is
      result : constant Prefetch_Pointer := new Prefetch_Type;
   begin
      result.mem        := mem;
      result.stride     := stride;
      result.multiplier := multiplier;
      return result;
   end Create_Prefetch;

   function Clone(mem : Prefetch_Type) return Memory_Pointer is
      result : constant Prefetch_Pointer := new Prefetch_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Read(mem      : in out Prefetch_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      cycles : Time_Type;
   begin

      -- Add any time left from the last prefetch.
      Advance(mem, mem.pending);

      -- Fetch the requested address.
      Start(mem.mem.all);
      Read(mem.mem.all, address, size);
      Commit(mem.mem.all, cycles);

      -- Prefetch the next address and save the time needed for the fetch.
      declare
         next_address : Address_Type;
      begin
         next_address := address * mem.multiplier + mem.stride;
         Start(mem.mem.all);
         Read(mem.mem.all, next_address, 1);
         Commit(mem.mem.all, mem.pending);
      end;

      -- Add the time required to fetch the requested address.
      Advance(mem, cycles);

   end Read;

   procedure Write(mem     : in out Prefetch_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      cycles : Time_Type;
   begin

      -- Add any time left from the last prefetch.
      Advance(mem, mem.pending);
      mem.pending := 0;

      -- Write the requested address.
      Start(mem.mem.all);
      Write(mem.mem.all, address, size);
      Commit(mem.mem.all, cycles);

      -- Update the time.
      Advance(mem, cycles);

      mem.writes := mem.writes + 1;

   end Write;

   procedure Idle(mem      : in out Prefetch_Type;
                  cycles   : in Time_Type) is
   begin
      if cycles > mem.pending then
         mem.pending := 0;
      else
         mem.pending := mem.pending - cycles;
      end if;
      Advance(mem, cycles);
   end Idle;

   procedure Show_Access_Stats(mem : in out Prefetch_Type) is
   begin
      Show_Access_Stats(mem.mem.all);
   end Show_Access_Stats;

   function To_String(mem : Prefetch_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(prefetch ");
      Append(result, "(stride" & Address_Type'Image(mem.stride) & ")");
      Append(result, "(multiplier" & Address_Type'Image(mem.multiplier) & ")");
      Append(result, "(memory ");
      Append(result, To_String(mem.mem.all));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Prefetch_Type) return Cost_Type is
   begin
      return Get_Cost(mem.mem.all);
   end Get_Cost;

   procedure Adjust(mem : in out Prefetch_Type) is
   begin
      mem.mem := Clone(mem.mem.all);
   end Adjust;

   procedure Finalize(mem : in out Prefetch_Type) is
   begin
      Destroy(Memory_Pointer(mem.mem));
   end Finalize;

end Memory.Prefetch;
