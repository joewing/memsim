
with Ada.Text_IO; use Ada.Text_IO;

package body Distribution is

   min_size : constant Address_Type := 4096;

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
         a2 : constant Address_Type := a1 + Address_Type(a.size) + min_size;
         b1 : constant Address_Type := b.start;
         b2 : constant Address_Type := b1 + Address_Type(b.size) + min_size;
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
            r     : Range_Type := dist.ranges.Element(i);
            rsize : constant Address_Type := Address_Type(r.size);
            rend  : constant Address_Type := r.start + rsize;
         begin
            if address >= r.start and address < rend + min_size then
               if address + Address_Type(size) > rend then
                  -- This address extends the range.
                  r.size := Positive(address + Address_Type(size) - r.start);
                  dist.ranges.Replace_Element(i, r);
               end if;
               return;
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
                  dist.ranges.Delete(i);
               end if;
            end;
         end loop;

         -- Insert a new range.
         dist.ranges.Append(r);
      end;

   end Insert;

   procedure Push_Limit(dist  : in out Distribution_Type;
                        lower : in Address_Type;
                        upper : in Address_Type) is
      l : constant Limit_Type := Limit_Type'(trans => null,
                                             lower => lower,
                                             upper => upper);
   begin
      dist.limits.Append(l);
   end Push_Limit;

   procedure Pop_Limit(dist : in out Distribution_Type) is
   begin
      dist.limits.Delete_Last;
   end Pop_Limit;

   procedure Push_Transform(dist  : in out Distribution_Type;
                            trans : in Applicative_Pointer) is
      l : constant Limit_Type := Limit_Type'(trans => trans,
                                             lower => 0,
                                             upper => 0);
   begin
      dist.limits.Append(l);
   end Push_Transform;

   procedure Pop_Transform(dist : in out Distribution_Type) is
   begin
      dist.limits.Delete_Last;
   end Pop_Transform;

   function Get_Weighted_Value(dist       : Distribution_Type;
                               start      : Address_Type;
                               size       : Positive;
                               alignment  : Positive) return Address_Type is

      addr  : Address_Type := start;
      nsize : Natural := size;

   begin

      loop
         if nsize <= alignment then
            return addr;
         end if;
         case RNG.Random(dist.generator) mod 8 is
            when 0 =>      -- Use the first address.
               return addr;
            when 1 =>      -- Use the last address.
               return addr + Address_Type(nsize - alignment);
            when 2 .. 4 => -- Lower half of the range.
               nsize := (nsize + 1) / 2;
            when others => -- Upper half of the range.
               addr  := addr + Address_Type(nsize / 2);
               nsize := (nsize + 1) / 2;
         end case;
      end loop;

   end Get_Weighted_Value;

   function Random_Address(dist        : Distribution_Type;
                           alignment   : Positive) return Address_Type is

      r     : Range_Type;
      addr  : Address_Type;
      valid : Boolean;
      count : Natural;

   begin

      -- We make multiple attempts to pick a valid address, otherwise
      -- we settle for any address even if it is not valid.
      -- It is possible that no valid addresses exist.
      count := 100;
      loop

         -- Select a random range.
         declare
            i : Natural := RNG.Random(dist.generator);
         begin
            i := i mod Natural(dist.ranges.Length);
            r := dist.ranges.Element(i + 1);
         end;

         -- Select an address in the range.
         addr := Get_Weighted_Value(dist, r.start, r.size, alignment);

         -- Transform the address and check validity.
         valid := True;
         for i in 1 .. Integer(dist.limits.Length) loop
            declare
               l : constant Limit_Type := dist.limits.Element(i);
            begin
               if l.trans /= null then
                  addr := Apply(l.trans.all, addr, True);
               elsif addr < l.lower or addr > l.upper then
                  valid := False;
               end if;
            end;
         end loop;

         -- Enforce alignment.
         -- This could push the addres outside of the range.
         addr := addr - (addr mod Address_Type(alignment));

         -- Exit if we have a valid address or exceeded the
         -- max number of iterations.
         count := count - 1;
         exit when valid or count = 0;

      end loop;

      return addr;

   end Random_Address;

   function Random(dist : Distribution_Type) return Natural is
   begin
      return RNG.Random(dist.generator);
   end Random;

   procedure Print(dist : in Distribution_Type) is
      size : Natural := 0;
   begin
      Put_Line("Ranges:");
      for i in 1 .. Integer(dist.ranges.Length) loop
         Put_Line("  " & Address_Type'Image(dist.ranges.Element(i).start) &
                  ": " & Natural'Image(dist.ranges.Element(i).size));
         size := size + dist.ranges.Element(i).size;
      end loop;
      Put_Line("Size: " & Natural'Image(size));
   end Print;

end Distribution;
