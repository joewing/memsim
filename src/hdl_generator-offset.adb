
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Container;        use Memory.Container;

package body HDL_Generator.Offset is

   function Offset_Ports(mem : Memory_Pointer) return Port_Type is
      op    : constant Offset_Pointer  := Offset_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(op.all);
   begin
      return Get_Ports(other);
   end Offset_Ports;

   procedure Process_Offset(gen        : in out Generator_Type;
                            mem        : in Memory_Pointer;
                            word_bits  : in Positive;
                            addr_bits  : in Positive) is
      op       : constant Offset_Pointer  := Offset_Pointer(mem);
      other    : constant Memory_Pointer  := Get_Memory(op.all);
      name     : constant String := "m" & To_String(Get_ID(mem.all));
      oname    : constant String := "m" & To_String(Get_ID(other.all));
      offset   : constant Address_Type := Get_Offset(op.all);
   begin
      Process(gen, other, word_bits, addr_bits);
      Declare_Signals(gen, name, word_bits, addr_bits);
      Assign(gen, oname & "_addr",
             "std_logic_vector(unsigned(" & name & "_addr) + " &
             To_String(offset) & ")");
      Assign(gen, oname & "_din", name & "_din");
      Assign(gen, name & "_dout", oname & "_dout");
      Assign(gen, oname & "_re", name & "_re");
      Assign(gen, oname & "_we", name & "_we");
      Assign(gen, name & "_ready", oname & "_ready");
   end Process_Offset;

   procedure Register is
   begin
      Add_Type(Offset_Type'Tag, Offset_Ports'Access, Process_Offset'Access);
   end Register;

end HDL_Generator.Offset;
