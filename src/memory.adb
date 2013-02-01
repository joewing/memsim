
package body Memory is

   procedure Start(mem : in out Memory_Type) is
   begin
      mem.transactions.Append(mem.time);
   end Start;

   procedure Commit(mem    : in out Memory_Type;
                    cycles : out Time_Type) is
      start_time : Time_Type;
   begin
      start_time := mem.transactions.Last_Element;
      cycles := mem.time - start_time;
      mem.transactions.Delete_Last;
   end Commit;

   procedure Idle(mem      : in out Memory_Type;
                  cycles   : in Time_Type) is
   begin
      Advance(mem, cycles);
   end Idle;

   function Get_Time(mem : Memory_Type) return Time_Type is
   begin
      return mem.time;
   end Get_Time;

   procedure Advance(mem      : in out Memory_type;
                     cycles   : in Time_Type) is
   begin
      mem.time := mem.time + cycles;
   end Advance;

end Memory;

