
package body Memory.Transform.Shift is

   function Create_Shift(mem     : access Memory_Type'Class;
                         shift   : Integer) return Shift_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      Set_Memory(result.all, mem);
      if shift < 0 then
         result.shift := Natural(Address_Type'Size + shift);
      else
         result.shift := Natural(shift);
      end if;
      return result;
   end Create_Shift;

   function Random_Shift(generator  : RNG.Generator;
                         max_cost   : Cost_Type) return Memory_Pointer is
      result : Shift_Pointer := new Shift_Type;
   begin
      if Get_Cost(result.all) > max_cost then
         Destroy(Memory_Pointer(result));
         return null;
      end if;
      result.shift := 1;
      return Memory_Pointer(result);
   end Random_Shift;

   function Get_Shift(mem : Shift_Type) return Natural is
   begin
      return mem.shift;
   end Get_Shift;

   procedure Set_Shift(mem    : in out Shift_Type;
                       shift  : in Natural) is
   begin
      mem.shift := shift mod Address_Type'Size;
   end Set_Shift;

   function Clone(mem : Shift_Type) return Memory_Pointer is
      result : constant Shift_Pointer := new Shift_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Shift_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      if mem.shift = 0 then
         mem.shift := mem.shift + 1;
      elsif mem.shift = Address_Type'Size then
         mem.shift := mem.shift - 1;
      elsif (RNG.Random(generator) mod 2) = 0 then
         mem.shift := mem.shift + 1;
      else
         mem.shift := mem.shift - 1;
      end if;
   end Permute;

   function Apply(mem      : Shift_Type;
                  address  : Address_Type) return Address_Type is
      right    : constant Address_Type := address * (2 ** mem.shift);
      lshift   : constant Natural := Address_Type'Size - mem.shift;
      left     : constant Address_Type := address / (2 ** lshift);
   begin
      return right or left;
   end Apply;

   function To_String(mem : Shift_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(transform ");
      Append(result, "(shift" & Natural'Image(mem.shift) & ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Shift_Type) return Cost_Type is
   begin
      return Get_Cost(Container_Type(mem));
   end Get_Cost;

end Memory.Transform.Shift;
