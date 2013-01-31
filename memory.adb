
package body Memory is

   procedure Start(mem : in out Memory_Type) is
   begin
      mem.transactions.Append(mem.time);
   end Start;

   procedure Commit(mem    : in out Memory_Type;
                    cycles : out Natural) is
      start_time : Natural;
   begin
      start_time := mem.transactions.Last_Element;
      cycles := mem.time - start_time;
      mem.transactions.Delete_Last;
   end Commit;

   function Get_Time(mem : Memory_Type) return Natural is
   begin
      return mem.time;
   end Get_Time;

   procedure Advance(mem      : in out Memory_type;
                     cycles   : Natural) is
   begin
      mem.time := mem.time + cycles;
   end Advance;

end Memory;

