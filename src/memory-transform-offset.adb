
package body Memory.Transform.Offset is

   function Create_Offset return Offset_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      return result;
   end Create_Offset;

   function Random_Offset(next      : access Memory_Type'Class;
                          generator : RNG.Generator;
                          max_cost  : Cost_Type) return Memory_Pointer is
      result : constant Offset_Pointer := new Offset_Type;
   begin
      Set_Memory(result.all, next);
      if (RNG.Random(generator) mod 2) = 0 then
         result.offset := 0 - Address_Type(1);
      else
         result.offset := 1;
      end if;
      return Memory_Pointer(result);
   end Random_Offset;

   function Get_Offset(mem : Offset_Type) return Address_Type is
   begin
      return mem.offset;
   end Get_Offset;

   procedure Set_Offset(mem      : in out Offset_Type;
                        offset   : in Address_Type) is
   begin
      mem.offset := offset;
   end Set_Offset;

   function Clone(mem : Offset_Type) return Memory_Pointer is
      result : constant Offset_Pointer := new Offset_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Offset_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type) is
   begin
      if (RNG.Random(generator) mod 2) = 0 then
         mem.offset := mem.offset + 1;
      else
         mem.offset := mem.offset - 1;
      end if;
   end Permute;

   function Apply(mem      : Offset_Type;
                  address  : Address_Type;
                  dir      : Boolean) return Address_Type is
   begin
      if dir then
         return address + mem.offset;
      else
         return address - mem.offset;
      end if;
   end Apply;

   function To_String(mem : Offset_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(shift ");
      Append(result, "(value");
      if 0 - mem.offset < mem.offset then
         declare
            temp : constant String := Address_Type'Image(0 - mem.offset);
         begin
            Append(result, " -" & temp(temp'First + 1 .. temp'Last));
         end;
      else
         Append(result, Address_Type'Image(mem.offset));
      end if;
      Append(result, ")");
      Append(result, "(bank ");
      Append(result, To_String(To_String(mem.bank.all)));
      Append(result, ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   procedure Generate(mem  : in Offset_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      other       : constant Memory_Pointer  := Get_Memory(mem);
      name        : constant String := "m" & To_String(Get_ID(mem));
      oname       : constant String := "m" & To_String(Get_ID(other.all));
      offset      : constant Address_Type := mem.offset;
   begin
      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, word_bits);
      if (offset and 2 ** 63) /= 0 then
         Line(code, "   " & oname & "_addr <= std_logic_vector(unsigned(" &
              name & "_addr) - " & To_String(-offset) & ");");
      else
         Line(code, "   " & oname & "_addr <= std_logic_vector(unsigned(" &
              name & "_addr) + " & To_String(offset) & ");");
      end if;
      Line(code, "   " & oname & "_din <= " & name & "_din;");
      Line(code, "   " & name & "_dout <= " & oname & "_dout;");
      Line(code, "   " & oname & "_re <= " & name & "_re;");
      Line(code, "   " & oname & "_we <= " & name & "_we;");
      Line(code, "   " & name & "_ready <= " & oname & "_ready;");
   end Generate;

end Memory.Transform.Offset;
