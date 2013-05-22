
with Memory.Join; use Memory.Join;

package body HDL_Generator.Join is

   function Join_Ports(mem : Memory_Pointer) return Port_Type is
      result : Port_Type;
   begin
      return result;
   end Join_Ports;

   procedure Process_Join(gen       : in out Generator_Type;
                          mem       : in Memory_Pointer;
                          word_bits : in Positive;
                          addr_bits : in Positive) is
      name : constant String := "m" & To_String(Get_ID(mem.all));
   begin
      Signal(gen, name & "_addr", addr_bits);
      Signal(gen, name & "_din", word_bits);
      Signal(gen, name & "_dout", word_bits);
      Signal(gen, name & "_re");
      Signal(gen, name & "_we");
      Signal(gen, name & "_ready");
   end Process_Join;

   procedure Register is
   begin
      Add_Type(Join_Type'Tag, Join_Ports'Access, Process_Join'Access);
   end Register;

end HDL_Generator.Join;
