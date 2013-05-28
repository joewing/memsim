
with Memory; use Memory;

package HDL_Generator is

   function Generate(mem         : Memory_Pointer;
                     name        : String;
                     addr_bits   : Positive) return String;

end HDL_Generator;
