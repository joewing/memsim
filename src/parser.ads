
with Ada.Containers.Vectors;
use Ada.Containers;

with Memory;         use Memory;
with Memory.Split;   use Memory.Split;
with Lexer;          use Lexer;

package Parser is

   function Parse(file_name : String) return Memory_Pointer;

private

   type Split_Node is record
      split : Split_Pointer;
      count : Natural := 0;
   end record;

   package Split_Vectors is new Vectors(Natural, Split_Node);

   Parse_Error : exception;

   type Parser_Type is record
      lexer    : Lexer_Type;
      splits   : Split_Vectors.Vector;
   end record;

   procedure Raise_Error(parser  : in Parser_Type;
                         msg     : in String);

   function Get_Type(parser : Parser_Type) return Token_Type;

   function Get_Value(parser : Parser_Type) return String;

   procedure Match(parser  : in out Parser_Type;
                   token   : in Token_Type);

   function Parse_Boolean(value : String) return Boolean;

   procedure Parse_Memory(parser : in out Parser_Type;
                          result : out Memory_Pointer);

end Parser;
