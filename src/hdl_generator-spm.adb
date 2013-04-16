
with Memory.SPM; use Memory.SPM;
with Memory.Container; use Memory.Container;

package body HDL_Generator.SPM is

   function SPM_Ports(gen : Generator_Type;
                      mem : Memory_Pointer) return Port_Type is
      sp    : constant SPM_Pointer     := SPM_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(sp.all);
   begin
      return Get_Ports(gen, other);
   end SPM_Ports;

   function Process_SPM(gen : Generator_Type;
                        mem : Memory_Pointer) return String is
      sp    : constant SPM_Pointer     := SPM_Pointer(mem);
      other : constant Memory_Pointer  := Get_Memory(sp.all);
   begin
      return "SPM " & Generate(gen, other);
   end Process_SPM;

   procedure Register(gen : in out Generator_Type) is
   begin
      Add_Type(gen, SPM_Type'Tag, SPM_Ports'Access, Process_SPM'Access);
   end Register;

end HDL_Generator.SPM;
