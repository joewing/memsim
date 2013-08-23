
package body Benchmark.Cholesky is

   function Create_Cholesky return Benchmark_Pointer is
   begin
      return new Cholesky_Type;
   end Create_Cholesky;

   procedure Set_Argument(benchmark : in out Cholesky_Type;
                          arg       : in String) is
      value : constant String := Extract_Argument(arg);
   begin
      if Check_Argument(arg, "size") then
         benchmark.size := Positive'Value(value);
      else
         Set_Argument(Benchmark_Type(benchmark), arg);
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in out Cholesky_Type) is
      addr : Natural;
   begin
      for i in 0 .. benchmark.size - 1 loop
         addr := i * benchmark.size + i;
         Read(benchmark, Address_Type(addr) * 4, 4);
         for j in 0 .. i loop
            addr := i * benchmark.size + j;
            Read(benchmark, Address_Type(addr) * 4, 4);
         end loop;
         for j in i + 1 .. benchmark.size - 1 loop
            addr := i * benchmark.size + j;
            Read(benchmark, Address_Type(addr) * 4, 4);
            for k in 0 .. i loop
               addr := j * benchmark.size + k;
               Read(benchmark, Address_Type(addr) * 4, 4);
               addr := i * benchmark.size + k;
               Read(benchmark, Address_Type(addr) * 4, 4);
            end loop;
            addr := j * benchmark.size + i;
            Write(benchmark, Address_Type(addr) * 4, 4);
         end loop;
      end loop;
   end Run;

end Benchmark.Cholesky;
