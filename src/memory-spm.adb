
package body Memory.SPM is

   function Create_SPM(mem       : access Memory_Type'Class;
                       size      : Natural;
                       latency   : Time_Type := 1) return SPM_Pointer is
      result : constant SPM_Pointer := new SPM_Type;
   begin
      result.mem := mem;
      result.size := size;
      result.latency := latency;
      return result;
   end Create_SPM;

   procedure Read(mem      : in out SPM_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Start(mem.mem.all);
         Read(mem.mem.all, address, size);
         Commit(mem.mem.all, cycles);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            diff  : constant Address_Type := naddr - address;
            nsize : constant Positive := mem.size - Positive(diff);
         begin
            Start(mem.mem.all);
            Read(mem.mem.all, naddr, nsize);
            Commit(mem.mem.all, cycles);
         end;
      end if;
      Advance(mem, cycles);
   end Read;

   procedure Write(mem     : in out SPM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Start(mem.mem.all);
         Write(mem.mem.all, address, size);
         Commit(mem.mem.all, cycles);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            diff  : constant Address_Type := naddr - address;
            nsize : constant Positive := mem.size - Positive(diff);
         begin
            Start(mem.mem.all);
            Write(mem.mem.all, naddr, nsize);
            Commit(mem.mem.all, cycles);
         end;
      end if;
      Advance(mem, cycles);
   end Write;

   procedure Show_Access_Stats(mem : in SPM_Type) is
   begin
      Show_Access_Stats(mem.mem.all);
   end Show_Access_Stats;

   function To_String(mem : SPM_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(spm ");
      Append(result, "(size" & Natural'Image(mem.size) & ")");
      Append(result, "(latency" & Time_Type'Image(mem.latency) & ")");
      Append(result, "(memory ");
      Append(result, To_String(mem.mem.all));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   procedure Finalize(mem : in out SPM_Type) is
   begin
      Destroy(Memory_Pointer(mem.mem));
   end Finalize;

end Memory.SPM;
