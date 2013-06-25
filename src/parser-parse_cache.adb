
with Memory.Cache;
with Util; use Util;

separate (Parser)
procedure Parse_Cache(parser  : in out Parser_Type;
                      result  : out Memory_Pointer) is

   type Policy_Map_Type is record
      name     : Unbounded_String;
      policy   : Cache.Policy_Type;
   end record;

   type Policy_Map_Array is array(Positive range <>) of Policy_Map_Type;

   policies : constant Policy_Map_Array := (
      (To_Unbounded_String("lru"),        Cache.LRU),
      (To_Unbounded_String("mru"),        Cache.MRU),
      (To_Unbounded_String("fifo"),       Cache.FIFO),
      (To_Unbounded_String("plru"),       Cache.PLRU)
   );

   mem            : Memory_Pointer := null;
   line_count     : Positive := 1;
   line_size      : Positive := 8;
   associativity  : Positive := 1;
   latency        : Time_Type := 3;
   policy         : Cache.Policy_Type := Cache.LRU;
   write_back     : Boolean := True;

begin
   while Get_Type(parser) = Open loop
      Match(parser, Open);
      declare
         name : constant String := Get_Value(parser);
      begin
         Match(parser, Literal);
         if name = "memory" then
            if mem /= null then
               Raise_Error(parser,
                           "memory declared multipled times in cache");
            end if;
            Parse_Memory(parser, mem);
         else
            declare
               value : constant String := Get_Value(parser);
            begin
               Match(parser, Literal);
               if name = "line_count" then
                  line_count := Positive'Value(value);
               elsif name = "line_size" then
                  line_size := Positive'Value(value);
               elsif name = "associativity" then
                  associativity := Positive'Value(value);
               elsif name = "latency" then
                  latency := Time_Type'Value(value);
               elsif name = "write_back" then
                  write_back := Parse_Boolean(value);
               elsif name = "policy" then
                  declare
                     found : Boolean := False;
                  begin
                     for p in policies'Range loop
                        if policies(p).name = value then
                           policy := policies(p).policy;
                           found := True;
                           exit;
                        end if;
                     end loop;
                     if not found then
                        Raise_Error(parser,
                                    "invalid cache replacement policy");
                     end if;
                  end;
               else
                  Raise_Error(parser, "invalid cache attribute: " & name);
               end if;
            end;
         end if;
      end;
      Match(parser, Close);
   end loop;
   if mem = null then
      Raise_Error(parser, "no memory specified in cache");
   end if;
   result := Memory_Pointer(Cache.Create_Cache(mem,
                                               line_count,
                                               line_size,
                                               associativity,
                                               latency,
                                               policy,
                                               write_back));
exception
   when Data_Error | Constraint_Error =>
      Destroy(mem);
      Raise_Error(parser, "invalid value in cache");
   when Parse_Error =>
      Destroy(mem);
      raise Parse_Error;
end Parse_Cache;
