
package body Memory.Transform is

   procedure Process(mem      : in out Transform_Type;
                     address  : in Address_Type;
                     size     : in Address_Type;
                     is_write : in Boolean) is

      start    : Address_Type := address;
      trans    : Address_Type := Apply(Transform_Type'Class(mem), start);
      total    : Address_Type := 0;

      nsize    : Address_Type;
      last     : Address_Type;
      temp     : Address_Type;

   begin

      while total < size loop

         -- Determine how big we can make the current access.
         last := trans;
         nsize := 1;
         while total + nsize < size loop
            temp := Apply(Transform_Type'Class(mem), start + nsize);
            exit when last + 1 /= temp;
            last := temp;
            nsize := nsize + 1;
         end loop;

         -- Perform the access.
         if is_write then
            Write(Container_Type(mem), trans, Natural(nsize));
         else
            Read(Container_Type(mem), trans, Natural(nsize));
         end if;

         total := total + nsize;
         start := start + nsize;
         trans := temp;

      end loop;

   end Process;

   procedure Read(mem      : in out Transform_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Process(mem, address, Address_Type(size), False);
   end Read;

   procedure Write(mem     : in out Transform_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Process(mem, address, Address_Type(size), True);
   end Write;

end Memory.Transform;
