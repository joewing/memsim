
package body Memory.Transform.Offset is

   function Create_Offset(mem    : Memory_Pointer;
                          offset : Integer) return Offset_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      Set_Memory(result.all, mem);
      if offset < 0 then
         result.offset := 0 - Address_Type(-offset);
      else
         result.offset := Address_Type(offset);
      end if;
      return result;
   end Create_Offset;

   function Random_Offset(generator : RNG.Generator;
                          max_cost  : Cost_Type) return Memory_Pointer is
      result   : Offset_Pointer := new Offset_Type;
   begin
      if Get_Cost(result.all) > max_cost then
         Destroy(Memory_Pointer(result));
         return null;
      end if;
      if (RNG.Random(generator) mod 2) = 0 then
         result.offset := 0 - Address_Type(1);
      else
         result.offset := 1;
      end if;
      return Memory_Pointer(result);
   end Random_Offset;

   function Get_Offset(mem : Offset_Type) return Address_Type is
   begin
      return mem.offset;
   end Get_Offset;

   procedure Set_Offset(mem      : in out Offset_Type;
                        offset   : in Address_Type) is
   begin
      mem.offset := offset;
   end Set_Offset;

   function Clone(mem : Offset_Type) return Memory_Pointer is
      result : constant Offset_Pointer := new Offset_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Offset_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      if (RNG.Random(generator) mod 2) = 0 then
         mem.offset := mem.offset + 1;
      else
         mem.offset := mem.offset - 1;
      end if;
   end Permute;

   function Apply(mem      : Offset_Type;
                  address  : Address_Type) return Address_Type is
   begin
      return address + mem.offset;
   end Apply;

   function To_String(mem : Offset_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(transform ");
      Append(result, "(offset" & Address_Type'Image(mem.offset) & ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Offset_Type) return Cost_Type is
   begin
      return 28 * Address_Type'Size +
             Get_Cost(Container_Type(mem));
   end Get_Cost;

end Memory.Transform.Offset;
