
package body Benchmark.Heap is

   procedure Init(benchmark : in out Heap_Type) is
   begin
      Write(benchmark, 0, 0);
   end Init;

   procedure Insert(benchmark : in out Heap_Type;
                    value     : in Integer) is

      size  : constant Integer := Read(benchmark, 0) + 1;
      ptr   : Integer := size;
      next  : Integer;
 
   begin

      -- Place the new item at the end of the heap.
      Write(benchmark, size, value);

      -- Increase the size of the heap.
      Write(benchmark, 0, size);

      -- Restore the heap property.
      while ptr > 1 loop
         declare
            next  : constant Integer := ptr / 2;
            c     : constant Integer := Read(benchmark, ptr);
            p     : constant Integer := Read(benchmark, next);
         begin
            exit when c >= p;
            Write(benchmark, ptr, p);
            Write(benchmark, next, c);
            ptr := next;
         end;
      end loop;

   end Insert;

   procedure Run(benchmark : in out Heap_Type) is
   begin
      Init(benchmark);
   end Run;

end Benchmark.Heap;
