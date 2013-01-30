
package Clock is

   type Clock_Type is private;

   type Clock_Pointer is access Clock_Type;

   function Get_Time(c : Clock_Type) return Natural;

   procedure Advance(c        : Clock_Type;
                     cycles   : Natural := 1);

private

   type Clock_Type is record
      time : Natural := 0;
   end record;

end Clock;
