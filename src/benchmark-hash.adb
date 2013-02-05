
package body Benchmark.Hash is

   function Create_Hash return Benchmark_Pointer is
   begin
      return new Hash_Type;
   end Create_Hash;

   procedure Set_Argument(benchmark : in out Hash_Type;
                          arg       : in String) is
   begin
      if Check_Argument(arg, "size") then
         benchmark.size := Positive'Value(Extract_Argument(arg));
      elsif Check_Argument(arg, "iterations") then
         benchmark.iterations := Positive'Value(Extract_Argument(arg));
      else
         raise Invalid_Argument;
      end if;
   exception
      when others =>
         raise Invalid_Argument;
   end Set_Argument;

   procedure Run(benchmark : in out Hash_Type) is
   begin

      for i in 0 .. benchmark.size - 1 loop
         Write(benchmark, i, Get_Random(benchmark));
      end loop;

      for i in 1 .. benchmark.iterations loop
         declare
            rand  : constant Integer := Get_Random(benchmark);
            index : constant Natural := rand mod benchmark.size;
            temp  : Integer;
         begin
            temp := Read(benchmark, index);
            pragma Unreferenced(temp);
         end;
      end loop;

   end Run;

end Benchmark.Hash;
