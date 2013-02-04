
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
                  address  : in Address_Type) is
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Start(mem.mem.all);
         Read(mem.mem.all, address);
         Commit(mem.mem.all, cycles);
      end if;
      Advance(mem, cycles);
   end Read;

   procedure Write(mem     : in out SPM_Type;
                   address : in Address_Type) is
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Start(mem.mem.all);
         Write(mem.mem.all, address);
         Commit(mem.mem.all, cycles);
      end if;
      Advance(mem, cycles);
   end Write;

   procedure Show_Access_Stats(mem : in SPM_Type) is
   begin
      Show_Access_Stats(mem.mem.all);
   end Show_Access_Stats;

   procedure Finalize(mem : in out SPM_Type) is
   begin
      Destroy(Memory_Pointer(mem.mem));
   end Finalize;

end Memory.SPM;
