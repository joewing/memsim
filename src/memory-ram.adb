
package body Memory.RAM is

   function Create_RAM(latency    : Time_Type := 1;
                       word_size  : Positive  := 8;
                       word_count : Natural   := 65536) return RAM_Pointer is
      result : constant RAM_Pointer := new RAM_Type;
   begin
      result.latency    := latency;
      result.word_size  := word_size;
      result.word_count := word_count;
      return result;
   end Create_RAM;

   function Clone(mem : RAM_Type) return Memory_Pointer is
      result : constant RAM_Pointer := new RAM_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Reset(mem : in out RAM_Type) is
   begin
      Reset(Memory_Type(mem));
      mem.writes := 0;
   end Reset;

   procedure Read(mem      : in out RAM_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      word     : constant Address_Type := Address_Type(mem.word_size);
      offset   : constant Natural      := Natural(address mod word);
      count    : constant Positive     := (size + mem.word_size + offset - 1) /
                                          mem.word_size;
   begin
      Advance(mem, mem.latency * Time_Type(count));
   end Read;

   procedure Write(mem     : in out RAM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      word     : constant Address_Type := Address_Type(mem.word_size);
      offset   : constant Natural      := Natural(address mod word);
      count    : constant Positive     := (size + mem.word_size + offset - 1) /
                                          mem.word_size;
   begin
      Advance(mem, mem.latency * Time_Type(count));
      mem.writes := mem.writes + 1;
   end Write;

   function To_String(mem : RAM_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(ram ");
      Append(result, "(latency" & Time_Type'Image(mem.latency) & ")");
      Append(result, "(word_size" & Positive'Image(mem.word_size) & ")");
      Append(result, "(word_count" & Natural'Image(mem.word_count) & ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : RAM_Type) return Cost_Type is
   begin
      return 0;
   end Get_Cost;

   function Get_Writes(mem : RAM_Type) return Long_Integer is
   begin
      return mem.writes;
   end Get_Writes;

   function Get_Word_Size(mem : RAM_Type) return Positive is
   begin
      return mem.word_size;
   end Get_Word_Size;

   procedure Generate(mem  : in RAM_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      name        : constant String := "m" & To_String(Get_ID(mem));
      words       : constant Natural := mem.word_count;
      word_bits   : constant Natural := 8 * Get_Word_Size(mem);
      latency     : constant Time_Type := mem.latency;
   begin
      Declare_Signals(sigs, name, word_bits);
      Line(code, name & "_inst : entity work.ram");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH      => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH      => " & To_String(word_bits) & ",");
      Line(code, "      SIZE            => " & To_String(words) & ",");
      Line(code, "      LATENCY         => " & To_String(latency));
      Line(code, "   )");
      Line(code, "   port map (");
      Line(code, "      clk      => clk,");
      Line(code, "      rst      => rst,");
      Line(code, "      addr     => " & name & "_addr,");
      Line(code, "      din      => " & name & "_din,");
      Line(code, "      dout     => " & name & "_dout,");
      Line(code, "      re       => " & name & "_re,");
      Line(code, "      we       => " & name & "_we,");
      Line(code, "      mask     => " & name & "_mask,");
      Line(code, "      ready    => " & name & "_ready");
      Line(code, "   );");
   end Generate;

end Memory.RAM;
