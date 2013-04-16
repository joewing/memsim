
with Memory.RAM; use Memory.RAM;

package body HDL_Generator.RAM is

   function RAM_Ports(gen : Generator_Type;
                      mem : Memory_Pointer) return Port_Type is
      ram      : constant RAM_Pointer := RAM_Pointer(mem);
      result   : Port_Type;
   begin
      result.banks.Append(Get_Word_Size(ram.all));
      return result;
   end RAM_Ports;

   function Process_RAM(gen         : Generator_Type;
                        mem         : Memory_Pointer;
                        word_bits   : Positive;
                        addr_bits   : Positive) return String is
   begin
      return "RAM";
   end Process_RAM;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, RAM_Type'Tag, RAM_Ports'Access, Process_RAM'Access);
   end Register;

end HDL_Generator.RAM;
