
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package body Parser is

   procedure Parse_Arbiter(parser   : in out Parser_Type;
                           result   : out Memory_Pointer) is separate;

   procedure Parse_Cache(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_DRAM(parser : in out Parser_Type;
                        result : out Memory_Pointer) is separate;

   procedure Parse_Dup(parser : in out Parser_Type;
                       result : out Memory_Pointer) is separate;

   procedure Parse_EOR(parser : in out Parser_Type;
                       result : out Memory_Pointer) is separate;

   procedure Parse_Flash(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Flip(parser   : in out Parser_Type;
                        result   : out Memory_Pointer) is separate;

   procedure Parse_Join(parser   : in out Parser_Type;
                        result   : out Memory_Pointer) is separate;

   procedure Parse_Offset(parser : in out Parser_Type;
                          result : out Memory_Pointer) is separate;

   procedure Parse_Option(parser : in out Parser_Type;
                          result : out Memory_Pointer) is separate;

   procedure Parse_Perfect_Prefetch(parser   : in out Parser_Type;
                                    result   : out Memory_Pointer) is separate;

   procedure Parse_Prefetch(parser  : in out Parser_Type;
                            result  : out Memory_Pointer) is separate;

   procedure Parse_RAM(parser : in out Parser_Type;
                       result : out Memory_Pointer) is separate;

   procedure Parse_Register(parser  : in out Parser_Type;
                            result  : out Memory_Pointer) is separate;

   procedure Parse_Shift(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Split(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_SPM(parser : in out Parser_Type;
                       result : out Memory_Pointer) is separate;

   procedure Parse_Stats(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Super(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is separate;

   procedure Parse_Trace(parser  : in out Parser_Type;
                         result  : out Memory_Pointer) is separate;

   type Memory_Parser_Type is record
      name     : Unbounded_String;
      parser   : access procedure(parser  : in out Parser_Type;
                                  result  : out Memory_Pointer);
   end record;

   type Memory_Parser_Array is array(Positive range <>) of Memory_Parser_Type;

   parser_map : constant Memory_Parser_Array := (
      (To_Unbounded_String("arbiter"),          Parse_Arbiter'Access),
      (To_Unbounded_String("cache"),            Parse_Cache'Access),
      (To_Unbounded_String("dram"),             Parse_DRAM'Access),
      (To_Unbounded_String("dup"),              Parse_Dup'Access),
      (To_Unbounded_String("eor"),              Parse_EOR'Access),
      (To_Unbounded_String("flash"),            Parse_Flash'Access),
      (To_Unbounded_String("flip"),             Parse_Flip'Access),
      (To_Unbounded_String("join"),             Parse_Join'Access),
      (To_Unbounded_String("offset"),           Parse_Offset'Access),
      (To_Unbounded_String("option"),           Parse_Option'Access),
      (To_Unbounded_String("perfect_prefetch"), Parse_Perfect_Prefetch'Access),
      (To_Unbounded_String("prefetch"),         Parse_Prefetch'Access),
      (To_Unbounded_String("ram"),              Parse_RAM'Access),
      (To_Unbounded_String("register"),         Parse_Register'Access),
      (To_Unbounded_String("shift"),            Parse_Shift'Access),
      (To_Unbounded_String("split"),            Parse_Split'Access),
      (To_Unbounded_String("spm"),              Parse_SPM'Access),
      (To_Unbounded_String("stats"),            Parse_Stats'Access),
      (To_Unbounded_String("super"),            Parse_Super'Access),
      (To_Unbounded_String("trace"),            Parse_Trace'Access)
   );

   function Parse_Boolean(value : String) return Boolean is
   begin
      if value = "true" then
         return True;
      elsif value = "false" then
         return False;
      else
         raise Data_Error;
      end if;
   end Parse_Boolean;

   procedure Parse_Memory(parser : in out Parser_Type;
                          result : out Memory_Pointer) is
   begin
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser.lexer);
      begin
         Next(parser.lexer);
         for i in parser_map'First .. parser_map'Last loop
            if parser_map(i).name = name then
               parser_map(i).parser(parser, result);
               Match(parser, Close);
               return;
            end if;
         end loop;
         Raise_Error(parser, "invalid memory type: " & name);
      end;
   end Parse_Memory;

   function Parse(file_name : String) return Memory_Pointer is
      parser   : Parser_Type;
      result   : Memory_Pointer;
   begin
      Open(parser.lexer, file_name);
      Parse_Memory(parser, result);
      Match(parser, EOF);
      Close(parser.lexer);
      return result;
   exception
      when Name_Error =>
         return null;
      when Parse_Error =>
         return null;
   end Parse;

   procedure Raise_Error(parser  : in Parser_Type;
                         msg     : in String) is
      name : constant String := Get_File_Name(parser.lexer);
      line : constant String := Positive'Image(Get_Line(parser.lexer));
   begin
      Put_Line(name & "[" & line(2 .. line'Last) & "]: error: " & msg);
      raise Parse_Error;
   end Raise_Error;

   function Get_Type(parser : Parser_Type) return Token_Type is
   begin
      return Get_Type(parser.lexer);
   end Get_Type;

   function Get_Value(parser : Parser_Type) return String is
   begin
      return Get_Value(parser.lexer);
   end Get_Value;

   procedure Match(parser  : in out Parser_Type;
                   token   : in Token_Type) is
   begin
      if Get_Type(parser) /= token then
         Raise_Error(parser, "got '" & Get_Value(parser) &
                     "' expected '" & Token_Type'Image(token) & "'");
      end if;
      Next(parser.lexer);
   end Match;

   procedure Push_Wrapper(parser    : in out Parser_Type;
                          wrapper   : in Wrapper_Pointer;
                          count     : in Positive := 1) is
      last : constant Natural := Natural(count);
   begin
      parser.wrappers.Append(Wrapper_Node'(wrapper, 0, last));
   end Push_Wrapper;

   procedure Pop_Wrapper(parser  : in out Parser_Type;
                         wrapper : out Wrapper_Pointer;
                         index   : out Natural) is
   begin
      if parser.wrappers.Is_Empty then
         Raise_Error(parser, "unexpected join");
      end if;
      declare
         node  : Wrapper_Node := parser.wrappers.Last_Element;
      begin
         wrapper        := node.wrapper;
         index          := node.current;
         node.current   := node.current + 1;
         if node.current = node.last then
            Delete_Wrapper(parser);
         else
            parser.wrappers.Replace_Element(parser.wrappers.Last, node);
         end if;
      end;
   end Pop_Wrapper;

   procedure Delete_Wrapper(parser : in out Parser_Type) is
   begin
      parser.wrappers.Delete_Last;
   end Delete_Wrapper;

end Parser;
