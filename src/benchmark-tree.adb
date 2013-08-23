
package body Benchmark.Tree is

   function Create_Tree return Benchmark_Pointer is
   begin
      return new Tree_Type;
   end Create_Tree;

   procedure Set_Argument(benchmark : in out Tree_Type;
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

   procedure Insert(benchmark : in Tree_Type;
                    offset    : in out Natural;
                    value     : in Integer) is
      ptr   : Integer := 0;
      next  : Integer;
   begin
      Write_Value(benchmark, offset + 0, value);
      Write_Value(benchmark, offset + 4, -1);
      Write_Value(benchmark, offset + 8, -1);
      loop
         if value < Read_Value(benchmark, ptr) then
            next := ptr + 4;
         else
            next := ptr + 8;
         end if;
         ptr := Read_Value(benchmark, next);
         exit when ptr < 0;
      end loop;
      Write_Value(benchmark, next, offset);
      offset := offset + 12;
   end Insert;

   procedure Find(benchmark   : in Tree_Type;
                  value       : in Integer) is
      ptr   : Integer := 0;
      temp  : Integer;
   begin
      while ptr >= 0 loop
         temp := Read_Value(benchmark, ptr + 0);
         if value < temp then
            ptr := Read_Value(benchmark, ptr + 4);
         elsif value > temp then
            ptr := Read_Value(benchmark, ptr + 8);
         else
            return;
         end if;
      end loop;
   end Find;

   procedure Run(benchmark : in Tree_Type) is

      function Rand return Integer is
      begin
         return Get_Random(benchmark) mod benchmark.size;
      end Rand;


      offset : Natural := 0;

   begin

      -- Each node is 12 bytes:
      --    4 byte value
      --    4 byte left pointer
      --    4 byte right pointer

      -- Initialize the root.
      Write_Value(benchmark, offset + 0, Rand);
      Write_Value(benchmark, offset + 4, -1);
      Write_Value(benchmark, offset + 8, -1);
      offset := offset + 12;

      -- Build the tree (root is already set).
      for i in 2 .. benchmark.size loop
         Insert(benchmark, offset, Rand);
      end loop;

      -- Look up items in the tree.
      for i in 1 .. benchmark.iterations loop
         Find(benchmark, Rand);
      end loop;

   end Run;

end Benchmark.Tree;
