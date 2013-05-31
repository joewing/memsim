
with Memory.Join; use Memory.Join;

package body Memory.Transform is

   procedure Process(mem      : in out Transform_Type;
                     address  : in Address_Type;
                     size     : in Address_Type;
                     dir      : in Boolean;
                     is_read  : in Boolean) is

      start    : Address_Type := address;
      trans    : Address_Type := Apply(Transform_Type'Class(mem), start, dir);
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
            temp := Apply(Transform_Type'Class(mem), start + nsize, dir);
            exit when last + 1 /= temp;
            last := temp;
            nsize := nsize + 1;
         end loop;

         -- Perform the access.
         if dir then
            if is_read then
               Read(mem.bank.all, trans, Natural(nsize));
            else
               Write(mem.bank.all, trans, Natural(nsize));
            end if;
         else
            if is_read then
               Read(Container_Type(mem), trans, Natural(nsize));
            else
               Write(Container_Type(mem), trans, Natural(nsize));
            end if;
         end if;

         total := total + nsize;
         start := start + nsize;
         trans := temp;

      end loop;

   end Process;

   function Get_Bank(mem : Transform_Type) return Memory_Pointer is
   begin
      return Memory_Pointer(mem.bank);
   end Get_Bank;

   procedure Set_Bank(mem  : in out Transform_Type;
                      bank : access Memory_Type'Class) is
   begin
      mem.bank := bank;
   end Set_Bank;

   procedure Read(mem      : in out Transform_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Process(mem, address, Address_Type(size), True, True);
   end Read;

   procedure Write(mem     : in out Transform_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Process(mem, address, Address_Type(size), True, False);
   end Write;

   procedure Forward_Read(mem       : in out Transform_Type;
                          source    : in Natural;
                          address   : in Address_Type;
                          size      : in Positive) is
   begin
      Process(mem, address, Address_Type(size), False, True);
   end Forward_Read;

   procedure Forward_Write(mem      : in out Transform_Type;
                           source   : in Natural;
                           address  : in Address_Type;
                           size     : in Positive) is
   begin
      Process(mem, address, Address_Type(size), False, False);
   end Forward_Write;

   procedure Forward_Idle(mem    : in out Transform_Type;
                          source : in Natural;
                          cycles : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
   end Forward_Idle;

   function Forward_Get_Time(mem : Transform_Type) return Time_Type is
   begin
      return Get_Time(Container_Type(mem));
   end Forward_Get_Time;

   function Get_Cost(mem : Transform_Type) return Cost_Type is
      result : Cost_Type := Get_Cost(Container_Type(mem));
   begin
      result := result + Get_Cost(mem.bank.all);
      return result;
   end Get_Cost;

   procedure Adjust(mem : in out Transform_Type) is
      jp    : Join_Pointer;
      cp    : Container_Pointer;
      ptr   : Memory_Pointer;
   begin
      Adjust(Container_Type(mem));
      mem.bank := Clone(mem.bank.all);
      ptr := Memory_Pointer(mem.bank);
      loop
         if ptr.all in Join_Type'Class then
            jp := Join_Pointer(ptr);
            Set_Parent(jp.all, mem'Unrestricted_Access);
            exit;
         else
            cp := Container_Pointer(ptr);
            ptr := Get_Memory(cp.all);
         end if;
      end loop;
   end Adjust;

   procedure Finalize(mem : in out Transform_Type) is
   begin
      Finalize(Container_Type(mem));
      Destroy(Memory_Pointer(mem.bank));
   end Finalize;

end Memory.Transform;
