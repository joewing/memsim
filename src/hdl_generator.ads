
with Ada.Containers.Vectors;  use Ada.Containers;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Tags;                use Ada.Tags;

with Util;
with Memory; use Memory;

package HDL_Generator is

   type Port_Type is private;

   function Get_Ports(mem : Memory_Pointer) return Port_Type;

   function Generate(mem         : Memory_Pointer;
                     name        : String;
                     word_bits   : Positive;
                     addr_bits   : Positive) return String;

   function Get_Port_Count(port : Port_Type) return Natural;

   function Get_Width(port : Port_Type;
                      bank : Natural) return Positive;

private

   package Bank_Vectors is new Vectors(Natural, Positive);

   type Port_Type is record
      banks : Bank_Vectors.Vector;
   end record;

   type Generator_Type is limited record
      decl  : Unbounded_String;
      proc  : Unbounded_String;
      shift : Natural := 0;
   end record;

   type Ports_Function is
      access function(mem : Memory_Pointer) return Port_Type;

   type Process_Function is
      access procedure(gen        : in out Generator_Type;
                       mem        : in Memory_Pointer;
                       word_bits  : in Positive;
                       addr_bits  : in Positive);

   procedure Process(gen         : in out Generator_Type;
                     mem         : in Memory_Pointer;
                     word_bits   : in Positive;
                     addr_bits   : in Positive);

   procedure Add_Type(t       : in Tag;
                      ports   : in Ports_Function;
                      proc    : in Process_Function);

   procedure Assign(gen    : in out Generator_Type;
                    dest   : in String;
                    src    : in String);

   procedure Signal(gen    : in out Generator_Type;
                    name   : in String;
                    width  : in Natural);

   procedure Signal(gen    : in out Generator_Type;
                    name   : in String);

   procedure Declare_Signals(gen       : in out Generator_Type;
                             name      : in String;
                             word_bits : in Positive;
                             addr_bits : in Positive);

   procedure DLine(gen : in out Generator_Type;
                   str : in String := "");

   procedure PLine(gen : in out Generator_Type;
                   str : in String := "");

   function To_String(a : Address_Type) return String;

   function To_String(t : Time_Type) return String;

   function To_String(i : Integer) return String renames Util.To_String;

end HDL_Generator;
