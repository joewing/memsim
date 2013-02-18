
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body Parser is

   procedure Parse_Bank(lexer    : in out Lexer_Type;
                        result   : out Memory_Pointer) is separate;

   procedure Parse_Cache(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Dup(lexer  : in out Lexer_Type;
                       result : out Memory_Pointer) is separate;

   procedure Parse_Perfect_Prefetch(lexer    : in out Lexer_Type;
                                    result   : out Memory_Pointer) is separate;

   procedure Parse_Prefetch(lexer   : in out Lexer_Type;
                            result  : out Memory_Pointer) is separate;

   procedure Parse_RAM(lexer  : in out Lexer_Type;
                       result : out Memory_Pointer) is separate;

   procedure Parse_SPM(lexer  : in out Lexer_Type;
                       result : out Memory_Pointer) is separate;

   procedure Parse_Stats(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Super(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Trace(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Transform(lexer  : in out Lexer_Type;
                             result : out Memory_Pointer) is separate;

   type Memory_Parser_Type is record
      name     : Unbounded_String;
      parser   : access procedure(lexer   : in out Lexer_Type;
                                  result  : out Memory_Pointer);
   end record;

   type Memory_Parser_Array is array(Positive range <>) of Memory_Parser_Type;

   parser_map : constant Memory_Parser_Array := (
      (To_Unbounded_String("bank"),             Parse_Bank'Access),
      (To_Unbounded_String("cache"),            Parse_Cache'Access),
      (To_Unbounded_String("perfect_prefetch"), Parse_Perfect_Prefetch'Access),
      (To_Unbounded_String("prefetch"),         Parse_Prefetch'Access),
      (To_Unbounded_String("ram"),              Parse_RAM'Access),
      (To_Unbounded_String("stats"),            Parse_Stats'Access),
      (To_Unbounded_String("super"),            Parse_Super'Access),
      (To_Unbounded_String("spm"),              Parse_SPM'Access),
      (To_Unbounded_String("dup"),              Parse_Dup'Access),
      (To_Unbounded_String("transform"),        Parse_Transform'Access),
      (To_Unbounded_String("trace"),            Parse_Trace'Access)
   );

   procedure Parse_Memory(lexer  : in out Lexer_Type;
                          result : out Memory_Pointer) is
   begin
      Match(lexer, Open);
      declare
         name : constant String := Get_Value(lexer);
      begin
         Next(lexer);
         for i in parser_map'First .. parser_map'Last loop
            if parser_map(i).name = name then
               parser_map(i).parser(lexer, result);
               Match(lexer, Close);
               return;
            end if;
         end loop;
         Raise_Error(lexer, "invalid memory type: " & name);
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
   exception
      when Name_Error =>
         return null;
      when Parse_Error =>
         return null;
   end Parse;

   procedure Raise_Error(lexer   : in Lexer_Type;
                         msg     : in String) is
      name : constant String := Get_File_Name(lexer);
      line : constant String := Positive'Image(Get_Line(lexer));
   begin
      Put_Line(name & "[" & line(2 .. line'Last) & "]: error: " & msg);
      raise Parse_Error;
   end Raise_Error;

   procedure Match(lexer   : in out Lexer_Type;
                   token   : in Token_Type) is
   begin
      if Get_Type(lexer) /= token then
         Raise_Error(lexer, "got '" & Get_Value(lexer) & "' expected '" &
                     Token_Type'Image(token) & "'");
      end if;
      Next(lexer);
   end Match;

end Parser;
