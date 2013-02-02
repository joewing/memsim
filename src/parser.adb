
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Lexer; use Lexer;

package body Parser is

   Parse_Error : exception;

   procedure Match(lexer   : in out Lexer_Type;
                   token   : in Token_Type) is
   begin
      if Get_Type(lexer) /= token then
         Put_Line("ERROR: got '" & Get_Value(lexer) & "' expected '" &
                  Get_Value(lexer));
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

   type Memory_Parser_Type is record
      name     : Unbounded_String;
      parser   : access procedure(lexer   : in out Lexer_Type;
                                  result  : out Memory_Pointer);
   end record;

   type Memory_Parser_Array is array(Positive range <>) of Memory_Parser_Type;

   parser_map : constant Memory_Parser_Array := (
      (To_Unbounded_String("bank"),       Parse_Bank'access),
      (To_Unbounded_String("cache"),      Parse_Cache'access),
      (To_Unbounded_String("prefetch"),   Parse_Prefetch'access),
      (To_Unbounded_String("ram"),        Parse_RAM'access)
   );

   procedure Parse_Memory(lexer  : in out Lexer_Type;
                          result : out Memory_Pointer) is
   begin
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Next(lexer);
         for i in parser_map'first .. parser_map'last loop
            if parser_map(i).name = name then
               parser_map(i).parser(lexer, result);
               Match(lexer, Close);
               return;
            end if;
         end loop;
         Put_Line("ERROR: invalid memory type: " & name);
         raise Parse_Error;
      end;
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
