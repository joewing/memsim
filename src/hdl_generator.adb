
package body HDL_Generator is

   function Get_Ports(gen : Generator_Type;
                      mem : Memory_Pointer) return Port_Type is
      result : Port_Type;
   begin
      for i in gen.nodes.First_Index .. gen.nodes.Last_Index loop
         declare
            node : constant Generator_Node := gen.nodes.Element(i);
         begin
            if mem'Tag = node.t then
               return node.ports.all(gen, mem);
            end if;
         end;
      end loop;
      return result;
   end Get_Ports;

   function Generate(gen : Generator_Type;
                     mem : Memory_Pointer) return String is
   begin
      for i in gen.nodes.First_Index .. gen.nodes.Last_Index loop
         declare
            node : constant Generator_Node := gen.nodes.Element(i);
         begin
            if mem'Tag = node.t then
               return node.proc.all(gen, mem);
            end if;
         end;
      end loop;
      return "?";
   end Generate;

   function Get_Port_Count(port : Port_Type) return Natural is
   begin
      return Natural(port.banks.Length);
   end Get_Port_Count;

   function Get_Width(port : Port_Type;
                      bank : Natural) return Positive is
   begin
      return port.banks.Element(bank);
   end Get_Width;

   procedure Add_Type(gen     : in out Generator_Type;
                      t       : in Tag;
                      ports   : in Ports_Function;
                      proc    : in Process_Function) is
      node : constant Generator_Node := Generator_Node'(t, ports, proc);
   begin
      gen.nodes.Append(node);
   end Add_Type;

end HDL_Generator;
