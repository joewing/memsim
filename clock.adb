
package body Clock is

   function Get_Time(clock : Clock_Pointer) return Natural is
   begin
      return clock.time;
   end Get_Time;

   procedure Advance(clock    : Clock_Pointer;
                     cycles   : Natural := 1) is
   begin
      clock.time := clock.time + cycles;
   end Advance;

end Clock;
