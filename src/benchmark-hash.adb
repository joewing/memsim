
package body Benchmark.Hash is

   function Create_Hash return Benchmark_Pointer is
   begin
      return new Hash_Type;
   end Create_Hash;

   procedure Set_Argument(benchmark : in out Hash_Type;
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

   procedure Run(benchmark : in Hash_Type) is
   begin
      for i in 1 .. benchmark.iterations loop
         declare
            rand  : constant Integer := Get_Random(benchmark);
            index : constant Natural := rand mod benchmark.size;
         begin
            if (Get_Random(benchmark) mod 2) = 0 then
               Read(benchmark, Address_Type(index) * 4, 4);
            else
               Write(benchmark, Address_Type(index) * 4, 4);
            end if;
            Idle(benchmark, benchmark.spacing);
         end;
      end loop;
   end Run;

end Benchmark.Hash;
