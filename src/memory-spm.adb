
with BRAM;

package body Memory.SPM is

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

   function Random_SPM(generator : RNG.Generator;
                       max_cost  : Cost_Type)
                       return Memory_Pointer is
      result : SPM_Pointer := new SPM_Type;
   begin

      result.latency := 2;
      result.size := 1;
      for i in 1 .. 10 loop

         result.size := result.size * 2;
         if Get_Cost(result.all) > max_cost then
            result.size := result.size / 2;
            exit;
         end if;

         exit when Get_Cost(result.all) >= max_cost;

      end loop;

      if Get_Cost(result.all) > max_cost then
         Destroy(Memory_Pointer(result));
         return null;
      else
         return Memory_Pointer(result);
      end if;

   end Random_SPM;

   function Clone(mem : SPM_Type) return Memory_Pointer is
      result : constant SPM_Pointer := new SPM_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out SPM_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
      action : Natural := RNG.Random(generator) mod 2;
   begin

      for i in 1 .. 2 loop
         if action = 0 then         -- Increment.
            mem.size := mem.size * 2;
            exit when Get_Cost(mem) <= max_cost;
            mem.size := mem.size / 2;
         elsif mem.size > 1 then    -- Decrement.
            mem.size := mem.size / 2;
            exit;
         end if;
         action := (action + 1) mod 2;
      end loop;

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
      result := Cost_Type(BRAM.Get_Count(width, depth));
      result := result + Get_Cost(Container_Type(mem));
      return result;
   end Get_Cost;

   function Get_Size(mem : SPM_Type) return Natural is
   begin
      return mem.size;
   end Get_Size;

end Memory.SPM;
