
with Memory;   use Memory;
with Lexer;    use Lexer;

package Parser is

   function Parse(file_name : String) return Memory_Pointer;

private

   Parse_Error : exception;

   procedure Raise_Error(lexer   : in Lexer_Type;
                         msg     : in String);

   procedure Match(lexer   : in out Lexer_Type;
                   token   : in Token_Type);

   procedure Parse_Memory(lexer  : in out Lexer_Type;
                          result : out Memory_Pointer);

end Parser;
