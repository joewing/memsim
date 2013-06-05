
with Ada.Assertions; use Ada.Assertions;

package body Memory.Prefetch is

   function Create_Prefetch(mem        : access Memory_Type'Class;
                            stride     : Address_Type := 1)
                            return Prefetch_Pointer is
      result : constant Prefetch_Pointer := new Prefetch_Type;
   begin
      result.stride     := stride;
      Set_Memory(result.all, mem);
      return result;
   end Create_Prefetch;

   function Random_Prefetch(next       : access Memory_Type'Class;
                            generator  : RNG.Generator;
                            max_cost   : Cost_Type)
                            return Memory_Pointer is
      result   : Prefetch_Pointer := new Prefetch_Type;
      wsize    : constant Positive := Get_Word_Size(next.all);
   begin
      Set_Memory(result.all, next);
      if Get_Cost(result.all) > max_cost then
         Set_Memory(result.all, null);
         Destroy(Memory_Pointer(result));
         return Memory_Pointer(next);
      end if;
      result.stride := Address_Type(RNG.Random(generator) mod 3) - 1;
      result.stride := result.stride * Address_Type(wsize);
      return Memory_Pointer(result);
   end Random_Prefetch;

   function Clone(mem : Prefetch_Type) return Memory_Pointer is
      result : constant Prefetch_Pointer := new Prefetch_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Prefetch_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
      wsize : constant Positive := Get_Word_Size(mem);
   begin
      if (RNG.Random(generator) mod 2) = 0 then
         mem.stride := mem.stride + Address_Type(wsize);
      else
         mem.stride := mem.stride - Address_Type(wsize);
      end if;
   end Permute;

   procedure Reset(mem : in out Prefetch_Type) is
   begin
      Reset(Container_Type(mem));
      mem.pending := 0;
   end Reset;

   procedure Read(mem      : in out Prefetch_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin

      -- Add any time left from the last prefetch.
      Advance(mem, mem.pending);

      -- Fetch the requested address.
      Read(Container_Type(mem), address, size);

      -- Prefetch the next address and save the time needed for the fetch.
      declare
         next_address : constant Address_Type := address + mem.stride;
      begin
         Start(mem);
         Do_Read(mem, next_address, 1);
         Commit(mem, mem.pending);
      end;

   end Read;

   procedure Write(mem     : in out Prefetch_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin

      -- Add any time left from the last prefetch.
      Advance(mem, mem.pending);
      mem.pending := 0;

      -- Write the requested address.
      Write(Container_Type(mem), address, size);

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

   function Get_Time(mem : Prefetch_Type) return Time_Type is
   begin
      return Get_Time(Container_Type(mem)) + mem.pending;
   end Get_Time;

   function To_String(mem : Prefetch_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(prefetch ");
      Append(result, "(stride ");
      if (mem.stride and 2 ** 63) /= 0 then
         Append(result, "-" & To_String(-mem.stride));
      else
         Append(result, To_String(mem.stride));
      end if;
      Append(result, ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Prefetch_Type) return Cost_Type is
   begin
      return Get_Cost(Container_Type(mem));
   end Get_Cost;

   procedure Generate(mem  : in Prefetch_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
   begin
      Assert(False, "Memory.Prefetch.Generate not implemented");
   end Generate;

end Memory.Prefetch;
