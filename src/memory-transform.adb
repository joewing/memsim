
package body Memory.Transform is

   procedure Process(mem      : in out Transform_Type;
                     address  : in Address_Type;
                     size     : in Address_Type;
                     is_write : in Boolean) is

      offset      : Address_Type := 0;
      start       : Address_Type := address;
      transformed : Address_Type := Apply(Transform_Type'Class(mem), address);
      temp        : Address_Type;

   begin
      while offset <= size loop
         temp := Apply(Transform_Type'Class(mem), address + offset);
         if temp /= transformed + offset or else offset = size then
            declare
               nsize : constant Natural := Natural(address + offset - start);
            begin
               if is_write then
                  Write(Container_Type(mem), transformed, nsize);
               else
                  Read(Container_Type(mem), transformed, nsize);
               end if;
               start := address + offset;
               transformed := temp;
            end;
         end if;
         offset := offset + 1;
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
      Increment_Writes(mem);
   end Write;

end Memory.Transform;
