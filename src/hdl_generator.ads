
with Memory; use Memory;

-- Package for generating HDL.
package HDL_Generator is

   -- Generate HDL.
   -- This function returns a string containing VHDL.
   function Generate(mem         : Memory_Pointer;
                     name        : String;
                     addr_bits   : Positive) return String;

end HDL_Generator;
