
package Memory.RAM is

   type RAM_Type is new Memory_Type with private;

   type RAM_Pointer is access all RAM_Type'Class;

   function Create_RAM(latency      : Time_Type := 1;
                       burst        : Time_Type := 0;
                       word_size    : Positive  := 8;
                       word_count   : Natural   := 65536) return RAM_Pointer;

   overriding
   function Clone(mem : RAM_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem     : in out RAM_Type;
                   context : in Natural);

   overriding
   procedure Read(mem      : in out RAM_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out RAM_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   function To_String(mem : RAM_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : RAM_Type) return Cost_Type;

   overriding
   function Get_Writes(mem : RAM_Type) return Long_Integer;

   overriding
   function Get_Word_Size(mem : RAM_Type) return Positive;

   overriding
   function Get_Ports(mem : RAM_Type) return Port_Vector_Type;

   overriding
   procedure Generate(mem  : in RAM_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

private

   type RAM_Type is new Memory_Type with record
      latency     : Time_Type    := 1;
      burst       : Time_Type    := 0;
      word_size   : Positive     := 8;
      word_count  : Natural      := 65536;
      writes      : Long_Integer := 0;
   end record;

end Memory.RAM;
