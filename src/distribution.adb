
package body Distribution is

   procedure Set_Seed(dist : in out Distribution_Type;
                      seed : in Integer) is
   begin
      RNG.Reset(dist.generator, seed);
   end Set_Seed;

   procedure Insert(dist      : in out Distribution_Type;
                    address   : in Address_Type;
                    size      : in Positive) is

      function Check_Overlap(a, b : Range_Type) return Boolean is
         a1 : constant Address_Type := a.start;
         a2 : constant Address_Type := a1 + Address_Type(a.size);
         b1 : constant Address_Type := b.start;
         b2 : constant Address_Type := b1 + Address_Type(b.size);
      begin
         -- a1 b1 a2
         -- b1 a1 b2
         return  (a1 <= b1 and b1 <= a2) or (b1 <= a1 and a1 <= b2);
      end Check_Overlap;

      procedure Extend_Range(r : in out Range_Type;
                             o : in Range_Type) is
         r_end : constant Address_Type := r.start + Address_Type(r.size);
         o_end : constant Address_Type := o.start + Address_Type(o.size);
      begin
         r.start := Address_Type'Min(r.start, o.start);
         if o_end > r_end then
            r.size := r.size + Positive(o_end - r_end);
         end if;
      end Extend_Range;

   begin

      -- Check if this address already exists.
      for i in 1 .. Integer(dist.ranges.Length) loop
         declare
            r : Range_Type := dist.ranges.Element(i);
         begin
            if address >= r.start and
               address <= r.start + Address_Type(r.size) then
               if address + Address_Type(size) <=
                  r.start + Address_Type(r.size) then
                  -- This address is wholely contained.
                  return;
               else
                  -- This address extends the range.
                  r.size := Positive(address + Address_Type(size) - r.start);
                  dist.ranges.Replace_Element(i, r);
                  return;
               end if;
            end if;
         end;
      end loop;

      -- This is a new range.
      declare
         r : Range_Type := Range_Type'(address, size);
      begin

         -- Coalesce ranges with the new range.
         -- We start at the end of the vector to avoid moving stuff.
         for i in reverse 1 .. Integer(dist.ranges.Length) loop
            declare
               other : constant Range_Type := dist.ranges.Element(i);
            begin
               if Check_Overlap(r, other) then
                  Extend_Range(r, other);
                  dist.ranges.Set_Length(dist.ranges.Length - 1);
               end if;
            end;
         end loop;

         -- Insert a new range.
         dist.ranges.Append(r);
      end;

   end Insert;

   procedure Add_Transform(dist  : in out Distribution_Type;
                           trans : in Applicative_Pointer) is
   begin
      dist.transformations.Append(trans);
   end Add_Transform;

   procedure Reset_Transform(dist : in out Distribution_Type) is
   begin
      dist.transformations.Clear;
   end Reset_Transform;

   function Random_Address(dist        : Distribution_Type;
                           alignment   : Positive) return Address_Type is

      r     : Range_Type;
      addr  : Address_Type;

   begin

      -- Select a random range.
      declare
         i : constant Natural
            := RNG.Random(dist.generator) mod Natural(dist.ranges.Length);
      begin
         r := dist.ranges.Element(i);
      end;

      -- Select an address in the range.
      addr := r.start + Address_Type(RNG.Random(dist.generator) mod r.size);

      -- Transform the address.
      for i in 1 .. Integer(dist.transformations.Length) loop
         declare
            tp : constant Applicative_Pointer
               := dist.transformations.Element(i);
         begin
            addr := Apply(tp.all, addr, True);
         end;
      end loop;

      -- Enforce alignment.
      -- This could push the addres outside of the range.
      addr := addr - (addr mod Address_Type(alignment));

      return addr;

   end Random_Address;

   function Random(dist : Distribution_Type) return Natural is
   begin
      return RNG.Random(dist.generator);
   end Random;

end Distribution;
