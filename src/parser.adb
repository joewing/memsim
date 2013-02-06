
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Lexer; use Lexer;

with Memory.Stats;
with Memory.RAM;
with Memory.Bank;
with Memory.Cache;
with Memory.Prefetch;
with Memory.SPM;
with Memory.Dup;

package body Parser is

   Parse_Error : exception;

   procedure Parse_Memory(lexer  : in out Lexer_Type;
                          result : out Memory_Pointer);

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

   procedure Parse_Stats(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is
      mem : Memory_Pointer := null;
   begin
      if Get_Type(lexer) = Open then
         Parse_Memory(lexer, mem);
      end if;
      result := Memory_Pointer(Stats.Create_Stats(mem));
   end Parse_Stats;

   procedure Parse_RAM(lexer  : in out Lexer_Type;
                       result : out Memory_Pointer) is

      latency     : Time_Type := 1;
      word_size   : Positive  := 8;

   begin
      while Get_Type(lexer) = Open loop
         Match(lexer, Open);
         declare
            name : constant String := Get_Value(lexer);
         begin
            Match(lexer, Literal);
            declare
               value : constant String := Get_Value(lexer);
            begin
               Match(lexer, Literal);
               if name = "latency" then
                  latency := Time_Type'Value(value);
               elsif name = "word_size" then
                  word_size := Positive'Value(value);
               else
                  Raise_Error(lexer, "invalid ram attribute: " & value);
               end if;
            end;
         end;
         Match(lexer, Close);
      end loop;
      result := Memory_Pointer(RAM.Create_RAM(latency, word_size));
   exception
      when Data_Error =>
         Raise_Error(lexer, "invalid value in ram");
   end Parse_RAM;

   procedure Parse_Dup(lexer  : in out Lexer_Type;
                       result : out Memory_Pointer) is

      dp    : Dup.Dup_Pointer := Dup.Create_Dup;
      mem   : Memory_Pointer := null;

   begin
      while Get_Type(lexer) = Open loop
         Parse_Memory(lexer, mem);
         Dup.Add_Memory(dp.all, mem);
      end loop;
      result := Memory_Pointer(dp);
   exception
      when others =>
         Destroy(Memory_Pointer(dp));
         raise Parse_Error;
   end Parse_Dup;

   procedure Parse_SPM(lexer  : in out Lexer_Type;
                       result : out Memory_Pointer) is

      mem      : Memory_Pointer := null;
      size     : Natural := 0;
      latency  : Time_Type := 1;

   begin
      while Get_Type(lexer) = Open loop
         Match(lexer, Open);
         declare
            name : constant String := Get_Value(lexer);
         begin
            Match(lexer, Literal);
            if name = "memory" then
               if mem /= null then
                  Raise_Error(lexer, "memory declared multiple times in spm");
               end if;
               Parse_Memory(lexer, mem);
            else
               declare
                  value : constant String := Get_Value(lexer);
               begin
                  Match(lexer, Literal);
                  if name = "size" then
                     size := Natural'Value(value);
                  elsif name = "latency" then
                     latency := Time_Type'Value(value);
                  else
                     Raise_Error(lexer, "invalid spm attribute: " & name);
                  end if;
               end;
            end if;
         end;
         Match(lexer, Close);
      end loop;
      if mem = null then
         Raise_Error(lexer, "no memory specified in spm");
      end if;
      result := Memory_Pointer(SPM.Create_SPM(mem, size, latency));
   exception
      when Data_Error =>
         Raise_Error(lexer, "invalid value in spm");
   end Parse_SPM;

   procedure Parse_Cache(lexer   : in out Lexer_Type;
                         result  : out Memory_Pointer) is

      mem            : Memory_Pointer := null;
      line_count     : Positive := 1;
      line_size      : Positive := 1;
      associativity  : Positive := 1;
      latency        : Time_Type := 1;

   begin
      while Get_Type(lexer) = Open loop
         Match(lexer, Open);
         declare
            name : constant String := Get_Value(lexer);
         begin
            Match(lexer, Literal);
            if name = "memory" then
               if mem /= null then
                  Raise_Error(lexer,
                              "memory declared multipled times in cache");
               end if;
               Parse_Memory(lexer, mem);
            else
               declare
                  value : constant String := Get_Value(lexer);
               begin
                  Match(lexer, Literal);
                  if name = "line_count" then
                     line_count := Positive'Value(value);
                  elsif name = "line_size" then
                     line_size := Positive'Value(value);
                  elsif name = "associativity" then
                     associativity := Positive'Value(value);
                  elsif name = "latency" then
                     latency := Time_Type'Value(value);
                  else
                     Raise_Error(lexer, "invalid cache attribute: " & value);
                  end if;
               end;
            end if;
         end;
         Match(lexer, Close);
      end loop;
      if mem = null then
         Raise_Error(lexer, "no memory specified in cache");
      end if;
      result := Memory_Pointer(Cache.Create_Cache(mem,
                                                  line_count,
                                                  line_size,
                                                  associativity,
                                                  latency));
   exception
      when Data_Error =>
         Destroy(mem);
         Raise_Error(lexer, "invalid value in cache");
      when Parse_Error =>
         Destroy(mem);
         raise Parse_Error;
   end Parse_Cache;

   procedure Parse_Bank(lexer    : in out Lexer_Type;
                        result   : out Memory_Pointer) is
      bank  : Memory.Bank.Bank_Pointer := Memory.Bank.Create_Bank;
      mem   : Memory_Pointer := null;
      key   : Address_Type := 0;
      mask  : Address_Type := 1;
   begin
      while Get_Type(lexer) = Open loop
         Match(lexer, Open);
         mem   := null;
         key   := 0;
         mask  := 1;
         while Get_Type(lexer) = Open loop
            Match(lexer, Open);
            declare
               name  : constant String := Get_Value(lexer);
            begin
               Match(lexer, Literal);
               if name = "memory" then
                  if mem = null then
                     Parse_Memory(lexer, mem);
                  else
                     Destroy(mem);
                     Raise_Error(lexer, "duplicate memories in bank");
                  end if;
               else
                  declare
                     value : constant String := Get_Value(lexer);
                  begin
                     Match(lexer, Literal);
                     if name = "key" then
                        key := Address_Type'Value(value);
                     elsif name = "mask" then
                        mask := Address_Type'Value(value);
                     else
                        Raise_Error(lexer,
                                    "invalid attribute in bank: " & name);
                     end if;
                  end;
               end if;
            end;
            Match(lexer, Close);
         end loop;
         Memory.Bank.Add_Bank(bank.all, mem, key, mask);
         Match(lexer, Close);
      end loop;
      result := Memory_Pointer(bank);
   exception
      when Data_Error =>
         Destroy(Memory_Pointer(bank));
         Raise_Error(lexer, "invalid value in bank");
      when Parse_Error =>
         Destroy(Memory_Pointer(bank));
         raise Parse_Error;
   end Parse_Bank;

   procedure Parse_Prefetch(lexer   : in out Lexer_Type;
                            result  : out Memory_Pointer) is
      mem         : Memory_Pointer := null;
      stride      : Address_Type := 1;
      multiplier  : Address_Type := 1;
   begin
      while Get_Type(lexer) = Open loop
         Match(lexer, Open);
         declare
            name : constant String := Get_Value(lexer);
         begin
            Match(lexer, Literal);
            if name = "memory" then
               if mem /= null then
                  Raise_Error(lexer, "memory set multiple times in prefetch");
               end if;
               Parse_Memory(lexer, mem);
            else
               declare
                  value : constant String := Get_Value(lexer);
               begin
                  Match(lexer, Literal);
                  if name = "stride" then
                     stride := Address_Type'Value(value);
                  elsif name = "multiplier" then
                     multiplier := Address_Type'Value(value);
                  else
                     Raise_Error(lexer,
                                 "invalid attribute in prefetch: " & name);
                  end if;
               end;
            end if;
         end;
         Match(lexer, Close);
      end loop;
      if mem = null then
         Raise_Error(lexer, "memory not set in prefetch");
      end if;
      result := Memory_Pointer(Prefetch.Create_Prefetch(mem, stride,
                                                        multiplier));
   exception
      when Data_Error =>
         Destroy(mem);
         Raise_Error(lexer, "invalid value in prefetch");
      when Parse_Error =>
         Destroy(mem);
         raise Parse_Error;
   end Parse_Prefetch;

   type Memory_Parser_Type is record
      name     : Unbounded_String;
      parser   : access procedure(lexer   : in out Lexer_Type;
                                  result  : out Memory_Pointer);
   end record;

   type Memory_Parser_Array is array(Positive range <>) of Memory_Parser_Type;

   parser_map : constant Memory_Parser_Array := (
      (To_Unbounded_String("bank"),       Parse_Bank'Access),
      (To_Unbounded_String("cache"),      Parse_Cache'Access),
      (To_Unbounded_String("prefetch"),   Parse_Prefetch'Access),
      (To_Unbounded_String("ram"),        Parse_RAM'Access),
      (To_Unbounded_String("stats"),      Parse_Stats'Access),
      (To_Unbounded_String("spm"),        Parse_SPM'Access),
      (To_Unbounded_String("dup"),        Parse_Dup'Access)
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

end Parser;
