
with Memory.Join; use Memory.Join;

package body HDL_Generator.Join is

   function Join_Ports(gen : Generator_Type;
                       mem : Memory_Pointer) return Port_Type is
      result : Port_Type;
   begin
      return result;
   end Join_Ports;

   function Process_Join(gen        : Generator_Type;
                         mem        : Memory_Pointer;
                         word_bits  : Positive;
                         addr_bits  : Positive) return String is
      r : Unbounded_String;
   begin
      return To_String(r);
   end Process_Join;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, Join_Type'Tag, Join_Ports'Access, Process_Join'Access);
   end Register;

end HDL_Generator.Join;
