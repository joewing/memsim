
package Memory.Flash is

   type Flash_Type is new Memory_Type with private;

   type Flash_Pointer is access all Flash_Type'Class;

   function Create_Flash(word_size     : Positive  := 8;
                         block_size    : Positive  := 256;
                         read_latency  : Time_Type := 10;
                         write_latency : Time_Type := 1000)
                         return Flash_Pointer;

   overriding
   function Clone(mem : Flash_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem     : in out Flash_Type;
                   context : in Natural);

   overriding
   procedure Read(mem      : in out Flash_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Flash_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   function To_String(mem : Flash_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Flash_Type) return Cost_Type;

   overriding
   function Get_Writes(mem : Flash_Type) return Long_Integer;

   overriding
   function Get_Word_Size(mem : Flash_Type) return Positive;

   overriding
   function Get_Ports(mem : Flash_Type) return Port_Vector_Type;

   overriding
   procedure Generate(mem  : in Flash_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

private

   type Flash_Type is new Memory_Type with record
      word_size      : Positive     := 8;
      block_size     : Positive     := 256;
      read_latency   : Time_Type    := 10;
      write_latency  : Time_Type    := 1000;
      writes         : Long_Integer := 0;
   end record;

end Memory.Flash;
