
with Device;            use Device;
with Memory.Container;  use Memory.Container;

package body Memory.Transform.Offset is

   function Create_Offset return Offset_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      return result;
   end Create_Offset;

   function Random_Offset(next      : access Memory_Type'Class;
                          generator : Distribution_Type;
                          max_cost  : Cost_Type) return Memory_Pointer is
      result   : constant Offset_Pointer := Create_Offset;
      wsize    : constant Positive := Get_Word_Size(next.all);
      lwsize   : constant Long_Integer := Long_Integer(wsize);
      rand     : constant Natural := Random(generator);
   begin
      Set_Memory(result.all, next);

      if (rand mod 2) = 0 then
         -- Byte offset.
         result.value := Long_Integer(Random(generator)) mod lwsize;
         if ((rand / 2) mod 2) = 0 then
            result.value := -result.value;
         end if;
      else
         -- Word offset.
         declare
            temp : constant Address_Type := Random_Address(generator, wsize);
         begin
            if temp > Address_Type(Long_Integer'Last) then
               result.value := -Long_Integer(-temp);
            else
               result.value := Long_Integer(temp);
            end if;
         end;
      end if;

      return Memory_Pointer(result);
   end Random_Offset;

   function Clone(mem : Offset_Type) return Memory_Pointer is
      result : constant Offset_Pointer := new Offset_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Offset_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is
      wsize    : constant Positive := Get_Word_Size(mem);
      lwsize   : constant Long_Integer := Long_Integer(wsize);
      rand     : constant Natural := Random(generator);
   begin
      if abs(mem.value) < Long_Integer(wsize) then
         mem.value := Long_Integer(Random(generator)) mod lwsize;
         if ((rand / 2) mod 2) = 0 then
            mem.value := -mem.value;
         end if;
      else
         declare
            temp : constant Address_Type := Random_Address(generator, wsize);
         begin
            if temp > Address_Type(Long_Integer'Last) then
               mem.value := -Long_Integer(-temp);
            else
               mem.value := Long_Integer(temp);
            end if;
         end;
      end if;
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
      return mem.value = 0;
   end Is_Empty;

   function Get_Alignment(mem : Offset_Type) return Positive is
      alignment   : Positive := 1;
      wsize       : constant Positive := Get_Word_Size(mem);
   begin
      while (mem.value mod Long_Integer(alignment)) = 0 and
         alignment < wsize loop
         alignment := alignment * 2;
      end loop;
      return alignment;
   end Get_Alignment;

   function Get_Transform_Length(mem : Offset_Type) return Natural is
   begin
      return Get_Address_Bits;
   end Get_Transform_Length;

end Memory.Transform.Offset;
