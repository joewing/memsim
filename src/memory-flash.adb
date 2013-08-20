
with Ada.Assertions; use Ada.Assertions;

package body Memory.Flash is

   function Create_Flash(word_size     : Positive  := 8;
                         block_size    : Positive  := 256;
                         read_latency  : Time_Type := 10;
                         write_latency : Time_Type := 1000)
                         return Flash_Pointer is
      result : constant Flash_Pointer := new Flash_Type;
   begin
      result.block_size := block_size;
      result.read_latency := read_latency;
      result.write_latency := write_latency;
      return result;
   end Create_Flash;

   function Clone(mem : Flash_Type) return Memory_Pointer is
      result : constant Flash_Pointer := new Flash_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Reset(mem     : in out Flash_Type;
                   context : in Natural) is
   begin
      Reset(Memory_Type(mem), context);
      mem.writes := 0;
   end Reset;

   procedure Read(mem      : in out Flash_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      count : Positive := (size + mem.word_size - 1) / mem.word_size;
   begin
      if (address mod Address_Type(mem.word_size)) /= 0 then
         count := count + 1;
      end if;
      Advance(mem, Time_Type(count) * mem.read_latency);
   end Read;

   procedure Write(mem     : in out Flash_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      count : Positive := (size + mem.block_size - 1) / mem.block_size;
   begin
      if (address mod Address_Type(mem.block_size)) /= 0 then
         count := count + 1;
      end if;
      Advance(mem, Time_Type(count) * mem.write_latency);
      mem.writes := mem.writes + 1;
   end Write;

   function To_String(mem : Flash_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(flash ");
      Append(result, "(word_size" & Positive'Image(mem.word_size) & ")");
      Append(result, "(block_size" & Positive'Image(mem.block_size) & ")");
      Append(result, "(read_latency" &
             Time_Type'Image(mem.read_latency) & ")");
      Append(result, "(write_latency" &
             Time_Type'Image(mem.write_latency) & ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Flash_Type) return Cost_Type is
   begin
      return 0;
   end Get_Cost;

   function Get_Writes(mem : Flash_Type) return Long_Integer is
   begin
      return mem.writes;
   end Get_Writes;

   function Get_Word_Size(mem : Flash_Type) return Positive is
   begin
      return mem.word_size;
   end Get_Word_Size;

   function Get_Ports(mem : Flash_Type) return Port_Vector_Type is
      result   : Port_Vector_Type;
      port     : constant Port_Type := Get_Port(mem);
   begin
      result.Append(port);
      return result;
   end Get_Ports;

   procedure Generate(mem  : in Flash_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
   begin
      Assert(False, "Memory.Flash.Generate not implemented");
   end Generate;

end Memory.Flash;
