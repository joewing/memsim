
with Device;            use Device;
with Memory.Container;  use Memory.Container;

package body Memory.Transform.Shift is

   function Create_Shift return Shift_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      return result;
   end Create_Shift;

   function Random_Shift(next       : access Memory_Type'Class;
                         generator  : Distribution_Type;
                         max_cost   : Cost_Type) return Memory_Pointer is
      result   : constant Shift_Pointer := Create_Shift;
      abits    : constant Positive := Get_Address_Bits;
   begin
      Set_Memory(result.all, next);
      result.value := Long_Integer((Random(generator) mod abits)) + 1;
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
      abits : constant Positive := Get_Address_Bits;
   begin
      mem.value := Long_Integer((Random(generator) mod abits)) + 1;
   end Permute;

   function Get_Name(mem : Shift_Type) return String is
   begin
      return "shift";
   end Get_Name;

   function Apply(mem      : Shift_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      abits    : constant Integer      := Get_Address_Bits;
      wsize    : constant Address_Type := Address_Type(Get_Word_Size(mem));
      wbits    : constant Natural      := Log2(Natural(wsize)) - 1;
      caddr    : constant Address_Type := address mod wsize;
      saddr    : constant Address_Type := address / wsize;
      shift    : Long_Integer := mem.value;
      rmult    : Address_Type;
      lmult    : Address_Type;
      result   : Address_Type;
   begin
      if shift < 0 then
         shift := Long_Integer(abits - wbits) + shift;
      end if;
      shift := shift mod Long_Integer(abits - wbits);
      rmult := Address_Type(2) ** Natural(shift);
      lmult := Address_Type(2) ** (abits - Natural(shift) - wbits);
      if dir then
         result := ((saddr * rmult) or (saddr / lmult)) * wsize or caddr;
      else
         result := ((saddr * lmult) or (saddr / rmult)) * wsize or caddr;
      end if;
      return result and ((Address_Type(2) ** abits) - 1);
   end Apply;

   function Is_Empty(mem : Shift_Type) return Boolean is
   begin
      return mem.value = 0;
   end Is_Empty;

   function Get_Alignment(mem : Shift_Type) return Positive is
   begin
      return Get_Word_Size(mem);
   end Get_Alignment;

   function Get_Transform_Length(mem : Shift_Type) return Natural is
   begin
      return 0;
   end Get_Transform_Length;

end Memory.Transform.Shift;
