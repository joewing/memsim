
with Ada.Tags; use Ada.Tags;
with Ada.Containers.Vectors;
use Ada.Containers;

with Memory; use Memory;

package HDL_Generator is

   type Generator_Type is limited private;

   type Port_Type is private;

   function Get_Ports(gen : Generator_Type;
                      mem : Memory_Pointer) return Port_Type;

   function Generate(gen : Generator_Type;
                     mem : Memory_Pointer) return String;

   function Get_Port_Count(port : Port_Type) return Natural;

   function Get_Width(port : Port_Type;
                      bank : Natural) return Positive;

private

   package Bank_Vectors is new Vectors(Natural, Positive);

   type Port_Type is record
      banks : Bank_Vectors.Vector;
   end record;

   type Ports_Function is
      access function(gen : Generator_Type;
                      mem : Memory_Pointer) return Port_Type;

   type Process_Function is
      access function(gen : Generator_Type;
                      mem : Memory_Pointer) return String;

   type Generator_Node is record
      t        : Tag;
      ports    : Ports_Function;
      proc     : Process_Function;
   end record;

   package Generator_Vectors is new Vectors(Natural, Generator_Node);

   type Generator_Type is limited record
      nodes : Generator_Vectors.Vector;
   end record;

   procedure Add_Type(gen     : in out Generator_Type;
                      t       : in Tag;
                      ports   : in Ports_Function;
                      proc    : in Process_Function);

end HDL_Generator;
