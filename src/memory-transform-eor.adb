
package body Memory.Transform.EOR is

   function Create_EOR return EOR_Pointer is
      result : constant EOR_Pointer := new EOR_Type;
   begin
      return result;
   end Create_EOR;

   function Random_EOR(next      : access Memory_Type'Class;
                       generator : RNG.Generator;
                       max_cost  : Cost_Type) return Memory_Pointer is
      result   : constant EOR_Pointer := Create_EOR;
      abits    : constant Positive := Integer'Size - 1;
      bit      : constant Positive := 2 ** (RNG.Random(generator) mod abits);
   begin
      Set_Memory(result.all, next);
      result.value := bit;
      return Memory_Pointer(result);
   end Random_EOR;

   function Clone(mem : EOR_Type) return Memory_Pointer is
      result : constant EOR_Pointer := new EOR_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   function "xor"(a: Integer; b: Integer) return Integer is
      type MT is mod 2 ** Integer'Size;
      tempa : constant MT := MT'Mod(a);
      tempb : constant MT := MT'Mod(b);
   begin
      return Integer(tempa xor tempb);
   end "xor";

   procedure Permute(mem         : in out EOR_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
      abits : constant Positive := Integer'Size - 1;
      bit   : constant Positive := 2 ** (RNG.Random(generator) mod abits);
   begin
      mem.value := mem.value xor bit;
   end Permute;

   function Get_Name(mem : EOR_Type) return String is
   begin
      return "eor";
   end Get_Name;

   function Is_Empty(mem : EOR_Type) return Boolean is
   begin
      return mem.value = 0;
   end Is_Empty;

   function Apply(mem      : EOR_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
   begin
      return address xor Address_Type'Mod(mem.value);
   end Apply;

   function Get_Alignment(mem : EOR_Type) return Positive is
   begin
      for i in 0 .. 16 loop
         if (Address_Type'Mod(mem.value) and (2 ** i)) /= 0 then
            return Positive(2 ** i);
         end if;
      end loop;
      return 2 ** 16;
   end Get_Alignment;

end Memory.Transform.EOR;
