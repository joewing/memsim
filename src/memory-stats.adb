
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.Stats is

   function Create_Stats(mem : access Memory_Type'class)
                        return Stats_Pointer is
      result : constant Stats_Pointer := new Stats_Type;
   begin
      result.mem := mem;
      return result;
   end Create_Stats;

   procedure Read(mem      : in out Stats_Type;
                  address  : in Address_Type) is
      cycles : Time_Type := 0;
   begin
      if mem.mem /= null then
         Start(mem.mem.all);
         Read(mem.mem.all, address);
         Commit(mem.mem.all, cycles);
         Advance(mem, cycles);
      end if;
   end Read;

   procedure Write(mem     : in out Stats_Type;
                   address : in Address_Type) is
      cycles : Time_Type := 0;
   begin
      if mem.mem /= null then
         Start(mem.mem.all);
         Write(mem.mem.all, address);
         Commit(mem.mem.all, cycles);
         Advance(mem, cycles);
      end if;
   end Write;

   procedure Show_Stats(mem : in Stats_Type) is
   begin
      Show_Stats(Memory_Type(mem));
   end Show_Stats;

end Memory.Stats;
