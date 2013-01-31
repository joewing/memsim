
package body Memory.Prefetch is

   function Create_Prefetch(mem     : access Memory_Type'class;
                            stride  : Integer := 1) return Prefetch_Pointer is
      result : constant Prefetch_Pointer := new Prefetch_Type;
   begin
      result.mem     := mem;
      result.stride  := stride;
      return result;
   end Create_Prefetch;

   procedure Read(mem      : in out Prefetch_Type;
                  address  : Address_Type) is
      cycles : Time_Type;
   begin
      Start(mem.mem.all);
      Read(mem.mem.all, address);
      Commit(mem.mem.all, cycles);
      declare
         next_address : Address_Type;
      begin
         if mem.stride < 0 then
            next_address := address - Address_Type(-mem.stride);
         else
            next_address := address + Address_Type(mem.stride);
         end if;
         Read(mem.mem.all, next_address);
      end;
      Advance(mem, cycles);
   end Read;

   procedure Write(mem     : in out Prefetch_Type;
                   address : Address_Type) is
      cycles : Time_Type;
   begin
      Start(mem.mem.all);
      Write(mem.mem.all, address);
      Commit(mem.mem.all, cycles);
      Advance(mem, cycles);
   end Write;

end Memory.Prefetch;
