
package Clock is

   type Clock_Type is private;

   type Clock_Pointer is access Clock_Type;

   function Get_Time(clock : Clock_Pointer) return Natural;

   procedure Advance(clock    : Clock_Pointer;
                     cycles   : Natural := 1);

private

   type Clock_Type is record
      time : Natural := 0;
   end record;

end Clock;
