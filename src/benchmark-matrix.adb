
package body Benchmark.Matrix is

   procedure Set_Argument(benchmark : in out Matrix_Type;
                          arg       : in String) is
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "size") then
         benchmark.size := Positive'Value(value);
      elsif Check_Argument(arg, "iterations") then
         benchmark.iterations := Positive'Value(value);
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   function Get_Size(benchmark : Matrix_Type'Class) return Address_Type is
      size : constant Address_Type := Address_Type(benchmark.size);
   begin
      return size * size * 4;
   end Get_Size;

   procedure Read(benchmark   : in Matrix_Type'Class;
                  offset      : in Address_Type;
                  x, y        : in Natural) is
      addr : Address_Type := offset;
   begin
      addr := addr + Address_Type(x * benchmark.size + y);
      Read(Benchmark_Type(benchmark), addr, 4);
   end Read;

   procedure Write(benchmark  : in Matrix_Type'Class;
                   offset     : in Address_Type;
                   x, y       : in Natural) is
      addr : Address_Type := offset;
   begin
      addr := addr + Address_Type(x * benchmark.size + y);
      Write(Benchmark_Type(benchmark), addr, 4);
   end Write;

end Benchmark.Matrix;
