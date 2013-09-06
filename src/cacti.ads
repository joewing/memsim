
with Memory;   use Memory;
with Util;     use Util;

package CACTI is

   CACTI_Error : exception;

   function Get_Area(mem : Memory_Type'Class) return Cost_Type;

   function Get_Time(mem : Memory_Type'Class) return Time_Type;

end CACTI;
