
package body Benchmark.Stride is

   function Create_Stride return Benchmark_Pointer is
   begin
      return new Stride_Type;
   end Create_Stride;

   procedure Set_Argument(benchmark : in out Stride_Type;
                          arg       : in String) is
   begin
      if Check_Argument(arg, "size") then
         benchmark.size := Positive'Value(Extract_Argument(arg));
      elsif Check_Argument(arg, "stride") then
         benchmark.stride := Integer'Value(Extract_Argument(arg));
      elsif Check_Argument(arg, "iterations") then
         benchmark.iterations := Positive'Value(Extract_Argument(arg));
      else
         raise Invalid_Argument;
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in out Stride_Type) is
   begin

      for i in 0 .. benchmark.size - 1 loop
         Write(benchmark, i, Get_Random(benchmark));
      end loop;

      for i in 1 .. benchmark.iterations loop
         for offset in 0 .. benchmark.size - 1 loop
            declare
               temp : constant Integer := Read(benchmark, offset) + 1;
            begin
               Write(benchmark, offset, temp);
            end;
         end loop;
      end loop;

   end Run;

end Benchmark.Stride;
