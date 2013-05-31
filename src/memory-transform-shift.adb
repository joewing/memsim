
with Ada.Assertions; use Ada.Assertions;

package body Memory.Transform.Shift is

   function Create_Shift return Shift_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      return result;
   end Create_Shift;

   function Random_Shift(next       : access Memory_Type'Class;
                         generator  : RNG.Generator;
                         max_cost   : Cost_Type) return Memory_Pointer is
      result : constant Shift_Pointer := new Shift_Type;
   begin
      Set_Memory(result.all, next);
      result.shift := (RNG.Random(generator) mod 16) + 1;
      return Memory_Pointer(result);
   end Random_Shift;

   function Get_Shift(mem : Shift_Type) return Integer is
   begin
      return mem.shift;
   end Get_Shift;

   procedure Set_Shift(mem    : in out Shift_Type;
                       shift  : in Integer) is
   begin
      if shift < 0 then
         mem.shift := (Address_Type'Size - shift) mod Address_Type'Size;
      else
         mem.shift := shift mod Address_Type'Size;
      end if;
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
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      abits    : constant Integer      := Address_Type'Size;   -- FIXME
      shift    : constant Integer      := mem.shift;
      wsize    : constant Address_Type := Address_Type(Get_Word_Size(mem));
      caddr    : constant Address_Type := address mod wsize;
      saddr    : constant Address_Type := address / wsize;
      rmult    : constant Address_Type := Address_Type(2) ** shift;
      lmult    : constant Address_Type := Address_Type(2) ** (abits - shift);
   begin
      if dir then
         if lmult /= 0 then
            return (((saddr * rmult) or (saddr / lmult)) * wsize) or caddr;
         else
            return address;
         end if;
      else
         if rmult /= 0 then
            return (((saddr * lmult) or (saddr / rmult)) * wsize) or caddr;
         else
            return address;
         end if;
      end if;
   end Apply;

   function To_String(mem : Shift_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(shift ");
      Append(result, "(value" & Natural'Image(mem.shift) & ")");
      Append(result, "(bank ");
      Append(result, To_String(To_String(mem.bank.all)));
      Append(result, ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   procedure Generate(mem  : in Shift_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
   begin
      Assert(False, "Memory.Transform.Shift.Generate not implemented");
   end Generate;

end Memory.Transform.Shift;
