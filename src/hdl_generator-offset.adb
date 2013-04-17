
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Container;        use Memory.Container;

package body HDL_Generator.Offset is

   function Offset_Ports(gen : Generator_Type;
                         mem : Memory_Pointer) return Port_Type is
      op    : constant Offset_Pointer  := Offset_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(op.all);
   begin
      return Get_Ports(gen, other);
   end Offset_Ports;

   function Process_Offset(gen         : Generator_Type;
                           mem         : Memory_Pointer;
                           word_bits   : Positive;
                           addr_bits   : Positive) return String is
      op       : constant Offset_Pointer  := Offset_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(op.all);
      name     : constant String := "m" & To_String(Get_ID(mem.all));
      oname    : constant String := "m" & To_String(Get_ID(other.all));
      addr_str : constant String := "[" & To_String(addr_bits - 1) & ":0]";
      word_str : constant String := "[" & To_String(word_bits - 1) & ":0]";
      offset   : constant Address_Type := Get_Offset(op.all);
      r        : Unbounded_String;
   begin
      Append(r, Generate(gen, other, word_bits, addr_bits));
      Line(r, "wire " & addr_str & " " & name & "_addr;");
      Line(r, "wire " & word_str & " " & name & "_din;");
      Line(r, "wire " & word_str & " " & name & "_dout;");
      Line(r, "wire " & name & "_re;");
      Line(r, "wire " & name & "_we;");
      Line(r, "wire " & name & "_ready;");
      Line(r, "assign " & oname & "_addr = " & name & "_addr +" &
           Address_Type'Image(offset) & ";");
      Line(r, "assign " & oname & "_din = " & name & "_din;");
      Line(r, "assign " & name & "_dout = " & oname & "_dout;");
      Line(r, "assign " & oname & "_re = " & name & "_re;");
      Line(r, "assign " & oname & "_we = " & name & "_we;");
      Line(r, "assign " & name & "_ready = " & oname & "_ready;");
      return To_String(r);
   end Process_Offset;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, Offset_Type'Tag, Offset_Ports'Access,
               Process_Offset'Access);
   end Register;

end HDL_Generator.Offset;
