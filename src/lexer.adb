
with Ada.Characters.Latin_1;
use Ada.Characters;

package body Lexer is

   procedure Open(lexer       : out Lexer_Type;
                  file_name   : in String) is
   begin
      Character_IO.Open(File => lexer.file,
                        Mode => Character_IO.In_File,
                        Name => file_name);
      Next(lexer);
   end Open;

   procedure Close(lexer : in out Lexer_Type) is
   begin
      Character_IO.Close(lexer.file);
   end Close;

   procedure Get_Buffer_Token(lexer : in out Lexer_Type) is
      len : constant Natural := Length(lexer.buffer);
   begin
      if len = 0 then
         lexer.token := Invalid;
         lexer.value := Null_Unbounded_String;
      else
         case Element(lexer.buffer, 1) is
            when '('    =>
               lexer.token := Open;
               lexer.value := Head(lexer.buffer, 1);
               Delete(lexer.buffer, 1, 1);
            when ')'    =>
               lexer.token := Close;
               lexer.value := Head(lexer.buffer, 1);
               Delete(lexer.buffer, 1, 1);
            when others =>
               lexer.token := Literal;
               lexer.value := lexer.buffer;
               Delete(lexer.buffer, 1, len);
         end case;
      end if;
   end Get_Buffer_Token;

   function Is_Space(ch : Character) return Boolean is
   begin
      case ch is
         when ' ' | Latin_1.LF | Latin_1.CR | Latin_1.VT =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Space;

   function Is_Stop(ch : Character) return Boolean is
   begin
      if Is_Space(ch) then
         return True;
      else
         case ch is
            when '(' | ')' =>
               return True;
            when others =>
               return False;
         end case;
      end if;
   end Is_Stop;

   procedure Next(lexer : in out Lexer_Type) is
      ch          : Character;
      in_comment  : Boolean := False;
   begin
      loop
         loop
            Character_IO.Read(lexer.file, ch);
            if ch = ';' then
               in_comment := True;
            elsif ch = Latin_1.LF then
               lexer.line := lexer.line + 1;
               in_comment := False;
            end if;
            exit when not in_comment;
         end loop;
         if Is_Space(ch) then
            if Length(lexer.buffer) > 0 then
               Get_Buffer_Token(lexer);
               return;
            end if;
         else
            if Is_Stop(ch) and Length(lexer.buffer) > 0 then
               Get_Buffer_Token(lexer);
               Append(lexer.buffer, ch);
               return;
            else
               if Length(lexer.buffer) > 0 then
                  if Is_Stop(Element(lexer.buffer, 1)) then
                     Get_Buffer_Token(lexer);
                     Append(lexer.buffer, ch);
                     return;
                  end if;
               end if;
               Append(lexer.buffer, ch);
            end if;
         end if;
      end loop;
   exception
      when Character_IO.End_Error =>
         if Length(lexer.buffer) > 0 then
            Get_Buffer_Token(lexer);
         else
            lexer.token := EOF;
            lexer.value := To_Unbounded_String("<EOF>");
         end if;
   end Next;

   function Get_Type(lexer : Lexer_Type) return Token_Type is
   begin
      return lexer.token;
   end Get_Type;

   function Get_Value(lexer : Lexer_Type) return String is
   begin
      return To_String(lexer.value);
   end Get_Value;

   function Get_File_Name(lexer : Lexer_Type) return String is
   begin
      return Character_IO.Name(lexer.file);
   end Get_File_Name;

   function Get_Line(lexer : Lexer_Type) return Positive is
   begin
      return lexer.line;
   end Get_Line;

end Lexer;
