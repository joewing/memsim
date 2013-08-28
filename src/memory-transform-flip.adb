
with Device;            use Device;
with Memory.Container;  use Memory.Container;

package body Memory.Transform.Flip is

   function Create_Flip return Flip_Pointer is
      result : constant Flip_Pointer := new Flip_Type;
   begin
      return result;
   end Create_Flip;

   function Random_Flip(next        : access Memory_Type'Class;
                        generator   : Distribution_Type;
                        max_cost    : Cost_Type) return Memory_Pointer is
      result : constant Flip_Pointer := new Flip_Type;
   begin
      Set_Memory(result.all, next);
      return Memory_Pointer(result);
   end Random_Flip;

   function Clone(mem : Flip_Type) return Memory_Pointer is
      result : constant Flip_Pointer := new Flip_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Flip_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is
   begin
      null;
   end Permute;

   function Get_Name(mem : Flip_Type) return String is
   begin
      return "flip";
   end Get_Name;

   function Apply(mem      : Flip_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
      addr_bits   : constant Positive := Get_Address_Bits;
      word_bytes  : constant Natural := Get_Word_Size(mem);
      src_mask    : Address_Type;
      dest_mask   : Address_Type;
      result      : Address_Type := address mod Address_Type(word_bytes);
   begin
      src_mask    := Address_Type(2) ** (addr_bits - 1);
      dest_mask   := Address_Type(word_bytes);
      for i in 0 .. addr_bits - Log2(word_bytes) loop
         if (address and src_mask) /= 0 then
            result := result or dest_mask;
         end if;
         src_mask  := src_mask  / 2;
         dest_mask := dest_mask * 2;
      end loop;
      return result;
   end Apply;

   function Get_Alignment(mem : Flip_Type) return Positive is
   begin
      return Get_Word_Size(mem);
   end Get_Alignment;

   function Get_Transform_Length(mem : Flip_Type) return Natural is
   begin
      return 0;
   end Get_Transform_Length;

end Memory.Transform.Flip;
