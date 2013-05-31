
with Ada.Containers.Vectors;
use Ada.Containers;

with Memory;         use Memory;
with Memory.Wrapper; use Memory.Wrapper;
with Lexer;          use Lexer;

package Parser is

   function Parse(file_name : String) return Memory_Pointer;

private

   type Wrapper_Node is record
      wrapper  : Wrapper_Pointer;
      current  : Natural := 0;
      last     : Natural := 0;
   end record;

   package Wrapper_Vectors is new Vectors(Natural, Wrapper_Node);

   Parse_Error : exception;

   type Parser_Type is record
      lexer    : Lexer_Type;
      wrappers : Wrapper_Vectors.Vector;
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

   procedure Push_Wrapper(parser  : in out Parser_Type;
                          wrapper : in Wrapper_Pointer;
                          count   : in Positive := 1);

   procedure Pop_Wrapper(parser  : in out Parser_Type;
                         wrapper : out Wrapper_Pointer;
                         index   : out Natural);

   procedure Delete_Wrapper(parser : in out Parser_Type);

end Parser;
