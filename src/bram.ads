
-- Package to provide information on how memories can be implemented in BRAM.
-- This package relies on the Device package for information on BRAMs.
package BRAM is

   -- Get the number of BRAMs that would be required for the
   -- specified memory aspect ratio.
   function Get_Count(width : Natural;
                      depth : Natural) return Natural;

end BRAM;
