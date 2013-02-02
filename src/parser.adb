
with Ada.Text_IO; use Ada.Text_IO;
with Lexer; use Lexer;

package body Parser is

   Parse_Error : exception;

   procedure Match(lexer   : in out Lexer_Type;
                   token   : in Token_Type) is
   begin
      if Get_Type(lexer) /= token then
         Put_Line("Got " & Token_Type'image(Get_Type(lexer)) & " expected " &
                  Token_Type'image(token));
         raise Parse_Error;
      end if;
      Next(lexer);
   end Match;

   procedure Parse_RAM(lexer  : in out Lexer_Type;
                       result : out Memory_Pointer) is
   begin
      result := null;
   end Parse_RAM;

   procedure Parse_Cache(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is
   begin
      result := null;
   end Parse_Cache;

   procedure Parse_Bank(lexer    : in out Lexer_Type;
                        result   : out Memory_Pointer) is
   begin
      result := null;
   end Parse_Bank;

   procedure Parse_Prefetch(lexer   : in out Lexer_Type;
                            result  : out Memory_Pointer) is
   begin
      result := null;
   end Parse_Prefetch;

   procedure Parse_Memory(lexer  : in out Lexer_Type;
                          result : out Memory_Pointer) is
   begin
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Next(lexer);
         if    name = "ram" then
            Parse_RAM(lexer, result);
         elsif name = "cache" then
            Parse_Cache(lexer, result);
         elsif name = "bank" then
            Parse_Bank(lexer, result);
         elsif name = "prefetch" then
            Parse_Prefetch(lexer, result);
         else
            Put_Line("ERROR: invalid memory type: " & name);
            raise Parse_Error;
         end if;
      end;
      Match(lexer, Close);
   end Parse_Memory;

   function Parse(file_name : String) return Memory_Pointer is
      lexer    : Lexer_Type;
      result   : Memory_Pointer;
   begin
      Open(lexer, file_name);
      Parse_Memory(lexer, result);
      Match(lexer, EOF);
      Close(lexer);
      return result;
   end Parse;

end Parser;
