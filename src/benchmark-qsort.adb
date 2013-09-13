
package body Benchmark.QSort is

   function Create_QSort return Benchmark_Pointer is
   begin
      return new QSort_Type;
   end Create_QSort;

   procedure Set_Argument(benchmark : in out QSort_Type;
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

   procedure Sort(benchmark   : in QSort_Type;
                  left, right : in Integer) is
      a     : Integer := left;
      b     : Integer := right;
      av    : Integer;
      bv    : Integer;
      pivot : constant Integer := Read_Value(benchmark, left);
   begin
      loop
         while a <= right loop
            av := Read_Value(benchmark, a);
            exit when av >= pivot;
            a := a + 1;
         end loop;
         while b >= left loop
            bv := Read_Value(benchmark, b);
            exit when bv <= pivot;
            b := b - 1;
         end loop;
         exit when a > b;
         Write_Value(benchmark, a, bv);
         Write_Value(benchmark, b, av);
         a := a + 1;
         b := b - 1;
      end loop;
      if a - 1 > left then
         Sort(benchmark, left, a - 1);
      end if;
      if right > a then
         Sort(benchmark, a, right);
      end if;
   end Sort;

   procedure Run(benchmark : in QSort_Type) is
   begin

      -- Generate the data set.
      for i in 0 .. benchmark.size - 1 loop
         Write_Value(benchmark, i, Get_Random(benchmark));
      end loop;

      -- Sort in place.
      Sort(benchmark, 0, benchmark.size - 1);

   end Run;

end Benchmark.QSort;
