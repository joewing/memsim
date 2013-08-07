
with Ada.Containers.Vectors;
use Ada.Containers;

with Memory;         use Memory;
with Memory.Wrapper; use Memory.Wrapper;
with Lexer;          use Lexer;

-- Package to handle parsing memory descriptions.
package Parser is

   type Parser_Type is limited private;

   -- Parse the memory description in the specified file.
   function Parse(file_name : String) return Memory_Pointer;

private

   -- Exception for parse errors.
   Parse_Error : exception;

   -- Type to represent a wrapper.
   -- A wrapper is a memory component that fully contains one
   -- or more memory components.
   type Wrapper_Node is record
      wrapper  : Wrapper_Pointer;
      current  : Natural := 0;
      last     : Natural := 0;
   end record;

   -- Stack of wrappers.
   package Wrapper_Vectors is new Vectors(Natural, Wrapper_Node);

   type Parser_Type is limited record
      lexer    : Lexer_Type;
      wrappers : Wrapper_Vectors.Vector;
   end record;

   -- Raise a parser error with the specified message.
   procedure Raise_Error(parser  : in Parser_Type;
                         msg     : in String);

   -- Get the current token type.
   function Get_Type(parser : Parser_Type) return Token_Type;

   -- Get the current token value.
   function Get_Value(parser : Parser_Type) return String;

   -- Match the current token and move to the next.
   procedure Match(parser  : in out Parser_Type;
                   token   : in Token_Type);

   -- Parse a "true"/"false" string.
   function Parse_Boolean(value : String) return Boolean;

   -- Parse a memory subsystem.
   procedure Parse_Memory(parser : in out Parser_Type;
                          result : out Memory_Pointer);

   -- Push a wrapper onto the stack.
   procedure Push_Wrapper(parser  : in out Parser_Type;
                          wrapper : in Wrapper_Pointer;
                          count   : in Positive := 1);

   -- Pop a wrapper off of the stack.
   procedure Pop_Wrapper(parser  : in out Parser_Type;
                         wrapper : out Wrapper_Pointer;
                         index   : out Natural);

   -- Delete the top wrapper.
   procedure Delete_Wrapper(parser : in out Parser_Type);

end Parser;
