
package body Memory.Transform.Offset is

   function Create_Offset return Offset_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      return result;
   end Create_Offset;

   function Random_Offset(next      : access Memory_Type'Class;
                          generator : RNG.Generator;
                          max_cost  : Cost_Type) return Memory_Pointer is
      result   : constant Offset_Pointer := Create_Offset;
      wsize    : constant Natural := Get_Word_Size(next.all);
      base     : Integer;
   begin
      Set_Memory(result.all, next);

      if (RNG.Random(generator) mod 2) = 0 then
         -- Byte offset.
         base := RNG.Random(generator) mod wsize;
      else
         -- Word offset.
         base := 2 ** (RNG.Random(generator) mod 16);
      end if;

      if (RNG.Random(generator) mod 2) = 0 then
         -- Negative offset.
         result.value := -(wsize * base);
      else
         -- Positive offset.
         result.value := wsize * base;
      end if;

      return Memory_Pointer(result);
   end Random_Offset;

   function Clone(mem : Offset_Type) return Memory_Pointer is
      result : constant Offset_Pointer := new Offset_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Offset_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
      wsize : constant Natural := Get_Word_Size(mem);
   begin
      case RNG.Random(generator) mod 4 is
         when 0      => -- Add word offset
            mem.value := mem.value + wsize;
         when 1      => -- Subtract word offset.
            mem.value := mem.value - wsize;
         when 2      => -- Add byte offset
            mem.value := mem.value + 1;
         when others => -- Subtract byte offset
            mem.value := mem.value - 1;
      end case;
   end Permute;

   function Get_Name(mem : Offset_Type) return String is
   begin
      return "offset";
   end Get_Name;

   function Apply(mem      : Offset_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      offset : Address_Type;
   begin
      if mem.value < 0 then
         offset := 0 - Address_Type(-mem.value);
      else
         offset := Address_Type(mem.value);
      end if;
      if dir then
         return address + offset;
      else
         return address - offset;
      end if;
   end Apply;

   function Is_Empty(mem : Offset_Type) return Boolean is
   begin
      return Is_Empty(Transform_Type(mem)) or mem.value = 0;
   end Is_Empty;

   function Get_Alignment(mem : Offset_Type) return Positive is
      alignment   : Positive := 1;
   begin
      while (mem.value mod alignment) = 0 and alignment < 2 ** 16 loop
         alignment := alignment * 2;
      end loop;
      return alignment;
   end Get_Alignment;

end Memory.Transform.Offset;
