
with Util;        use Util;

package body Memory.Transform.Shift is

   function Create_Shift return Shift_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      return result;
   end Create_Shift;

   function Random_Shift(next       : access Memory_Type'Class;
                         generator  : Distribution_Type;
                         max_cost   : Cost_Type) return Memory_Pointer is
      result : constant Shift_Pointer := Create_Shift;
   begin
      Set_Memory(result.all, next);
      result.value := (Random(generator) mod 16) + 1;
      return Memory_Pointer(result);
   end Random_Shift;

   function Clone(mem : Shift_Type) return Memory_Pointer is
      result : constant Shift_Pointer := new Shift_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Shift_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is
   begin
      if mem.value = 0 then
         mem.value := mem.value + 1;
      elsif mem.value = Get_Address_Size(mem) * 8 then
         mem.value := mem.value - 1;
      elsif (Random(generator) mod 2) = 0 then
         mem.value := mem.value + 1;
      else
         mem.value := mem.value - 1;
      end if;
   end Permute;

   function Get_Name(mem : Shift_Type) return String is
   begin
      return "shift";
   end Get_Name;

   function Apply(mem      : Shift_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      abits    : constant Integer      := 8 * Get_Address_Size(mem);
      wsize    : constant Address_Type := Address_Type(Get_Word_Size(mem));
      wbits    : constant Natural      := Log2(Natural(wsize)) - 1;
      caddr    : constant Address_Type := address mod wsize;
      saddr    : constant Address_Type := address / wsize;
      shift    : Integer := mem.value;
      rmult    : Address_Type;
      lmult    : Address_Type;
      result   : Address_Type;
   begin
      if shift < 0 then
         shift := (abits - wbits + shift);
      end if;
      shift := shift mod (abits - wbits);
      rmult := Address_Type(2) ** shift;
      lmult := Address_Type(2) ** (abits - shift - wbits);
      if dir then
         result := ((saddr * rmult) or (saddr / lmult)) * wsize or caddr;
      else
         result := ((saddr * lmult) or (saddr / rmult)) * wsize or caddr;
      end if;
      return result and ((Address_Type(2) ** abits) - 1);
   end Apply;

   function Is_Empty(mem : Shift_Type) return Boolean is
   begin
      return Is_Empty(Transform_Type(mem)) or mem.value = 0;
   end Is_Empty;

   function Get_Alignment(mem : Shift_Type) return Positive is
   begin
      return Get_Word_Size(mem);
   end Get_Alignment;

end Memory.Transform.Shift;
