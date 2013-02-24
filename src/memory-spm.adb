
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

      result.latency := 1;
      result.size := 1;
      loop

         result.size := result.size * 2;
         if Get_Cost(result.all) > max_cost then
            result.size := result.size / 2;
            exit;
         end if;

         exit when (RNG.Random(generator) mod 8) = 0;

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
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Forward_Start(mem);
         Forward_Read(mem, address, size);
         Forward_Commit(mem, cycles);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            last  : constant Address_Type := address + Address_Type(size);
            nsize : constant Positive := Positive(last - naddr);
         begin
            Forward_Start(mem);
            Forward_Read(mem, naddr, nsize);
            Forward_Commit(mem, cycles);
         end;
      end if;
      Advance(mem, cycles);
   end Read;

   procedure Write(mem     : in out SPM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Forward_Start(mem);
         Forward_Write(mem, address, size);
         Forward_Commit(mem, cycles);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            diff  : constant Address_Type := naddr - address;
            nsize : constant Positive := mem.size - Positive(diff);
         begin
            Forward_Start(mem);
            Forward_Write(mem, naddr, nsize);
            Forward_Commit(mem, cycles);
         end;
      end if;
      Advance(mem, cycles);
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
   begin
      return 6 * 8 * Cost_Type(mem.size) + Get_Cost(Container_Type(mem));
   end Get_Cost;

end Memory.SPM;
