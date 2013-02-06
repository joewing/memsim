
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

end Memory.RAM;
