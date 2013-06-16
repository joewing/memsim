
with Memory.Transform; use Memory.Transform;

generic
   type T_Type (<>) is new Transform_Type with private;
   type T_Pointer is access all T_Type'Class;
   with function Create_Transform return T_Pointer;
package Parser.Transform_Parser is

   procedure Parse(parser : in out Parser_Type;
                   result : out Memory_Pointer);

end Parser.Transform_Parser;
