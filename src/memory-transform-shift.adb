
package body Memory.Transform.Shift is

   function Create_Shift(mem        : access Memory_Type'Class;
                         word_size  : Natural;
                         shift      : Integer) return Shift_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      Set_Memory(result.all, mem);
      if shift < 0 then
         result.shift := Natural(Address_Type'Size + shift);
      else
         result.shift := Natural(shift);
      end if;
      result.word_size := word_size;
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
      result.shift := (RNG.Random(generator) mod 16) + 1;
      result.word_size := 2 ** (RNG.Random(generator) mod 8);
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

   function Get_Word(mem : Shift_Type) return Natural is
   begin
      return mem.word_size;
   end Get_Word;

   procedure Set_Word(mem  : in out Shift_Type;
                      word : in Natural) is
   begin
      mem.word_size := word;
   end Set_Word;

   function Clone(mem : Shift_Type) return Memory_Pointer is
      result : constant Shift_Pointer := new Shift_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Shift_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      if (RNG.Random(generator) mod 2) = 0 then
         if mem.shift = 0 then
            mem.shift := mem.shift + 1;
         elsif mem.shift = Address_Type'Size then
            mem.shift := mem.shift - 1;
         elsif (RNG.Random(generator) mod 2) = 0 then
            mem.shift := mem.shift + 1;
         else
            mem.shift := mem.shift - 1;
         end if;
      else
         if mem.word_size > 1 and then (RNG.Random(generator) mod 2) = 0 then
            mem.word_size := mem.word_size / 2;
         else
            mem.word_size := mem.word_size * 2;
         end if;
      end if;
   end Permute;

   function Apply(mem      : Shift_Type;
                  address  : Address_Type) return Address_Type is
      wsize    : constant Address_Type := Address_Type(mem.word_size);
      caddr    : constant Address_Type := address mod wsize;
      saddr    : constant Address_Type := address / wsize;
      rshift   : constant Address_Type := Address_Type(2) ** mem.shift;
      right    : constant Address_Type := saddr * rshift;
      lshift   : constant Address_Type
                  := Address_Type(2) ** (Address_Type'Size - mem.shift);
   begin
      if lshift /= 0 then
         return ((right or (caddr / lshift)) * wsize) or caddr;
      else
         return (address * wsize) or caddr;
      end if;
   end Apply;

   function To_String(mem : Shift_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(transform ");
      Append(result, "(shift" & Natural'Image(mem.shift) & ")");
      Append(result, "(word_size" & Natural'Image(mem.word_size) & ")");
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
