
with Ada.Sequential_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Lexer is

   type Token_Type is (
      Invalid,
      Literal,
      Open,
      Close,
      EOF
   );

   type Lexer_Type is limited private;

   procedure Open(lexer       : out Lexer_Type;
                  file_name   : in String);

   procedure Close(lexer : in out Lexer_Type);

   procedure Next(lexer : in out Lexer_Type);

   function Get_Type(lexer : Lexer_Type) return Token_Type;

   function Get_Value(lexer : Lexer_Type) return String;

   function Get_File_Name(lexer : Lexer_Type) return String;

   function Get_Line(lexer : Lexer_Type) return Positive;

private

   package Character_IO is new Ada.Sequential_IO(Character);

   type Lexer_Type is limited record
      file     : Character_IO.File_Type;
      line     : Positive := 1;
      buffer   : Unbounded_String;
      token    : Token_Type := Invalid;
      value    : Unbounded_String;
   end record;

end Lexer;
