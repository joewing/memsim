
package body Memory.RAM is

   function Create_RAM(latency   : Time_Type := 1;
                       word_size : Positive  := 8) return RAM_Pointer is
      result : constant RAM_Pointer := new RAM_Type;
   begin
      result.latency := latency;
      result.word_size := word_size;
      return result;
   end Create_RAM;

   procedure Read(mem      : in out RAM_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      count : constant Positive := (size + mem.word_size - 1) / mem.word_size;
   begin
      Advance(mem, mem.latency * Time_Type(count));
   end Read;

   procedure Write(mem     : in out RAM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      count : constant Positive := (size + mem.word_size - 1) / mem.word_size;
   begin
      Advance(mem, mem.latency * Time_Type(count));
   end Write;

   function To_String(mem : RAM_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(ram ");
      Append(result, "(latency" & Time_Type'Image(mem.latency) & ")");
      Append(result, "(word_size" & Positive'Image(mem.word_size) & ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : RAM_Type) return Cost_Type is
   begin
      return 0;   -- FIXME
   end Get_Cost;

end Memory.RAM;
