
package Memory.RAM is

   type RAM_Type is new Memory_Type with private;

   type RAM_Pointer is access all RAM_Type'Class;

   function Create_RAM(latency   : Time_Type := 1;
                       word_size : Positive  := 8) return RAM_Pointer;

   overriding
   procedure Read(mem      : in out RAM_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out RAM_Type;
                   address : in Address_Type;
                   size    : in Positive);

private

   type RAM_Type is new Memory_Type with record
      latency     : Time_Type := 1;
      word_size   : Positive  := 8;
   end record;

end Memory.RAM;
