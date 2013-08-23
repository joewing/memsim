
package body Benchmark.Stride is

   function Create_Stride return Benchmark_Pointer is
   begin
      return new Stride_Type;
   end Create_Stride;

   procedure Set_Argument(benchmark : in out Stride_Type;
                          arg       : in String) is
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "size") then
         benchmark.size := Positive'Value(value);
      elsif Check_Argument(arg, "stride") then
         benchmark.stride := Integer'Value(value);
      elsif Check_Argument(arg, "iterations") then
         benchmark.iterations := Positive'Value(value);
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in Stride_Type) is
   begin
      for i in 1 .. benchmark.iterations loop
         for offset in 0 .. Address_Type(benchmark.size - 1) loop
            Read(benchmark, offset * 4, 4);
            if benchmark.spacing > 0 then
               Idle(benchmark, benchmark.spacing);
            end if;
         end loop;
      end loop;
   end Run;

end Benchmark.Stride;
