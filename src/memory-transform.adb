
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

      incr     : Address_Type;
      nsize    : Address_Type;
      last     : Address_Type;
      temp     : Address_Type;

   begin

      incr := Address_Type(Get_Alignment(Transform_Type'Class(mem)));
      while (address mod incr) /= 0 loop
         incr := incr / 2;
      end loop;

      while total < size loop

         -- Determine how big we can make the current access.
         last := trans;
         nsize := Address_Type'Min(size - total, incr);
         while total + nsize < size loop
            temp := Apply(Transform_Type'Class(mem), start + nsize, dir);
            exit when last + incr /= temp;
            last := temp;
            nsize := Address_Type'Min(size - total, nsize + incr);
         end loop;

         -- Perform the access.
         if dir and mem.bank /= null then
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

   function Get_Name(mem : Transform_Type) return String is
   begin
      return To_String(mem.name);
   end Get_Name;

   procedure Set_Value(mem    : in out Transform_Type;
                       value  : in Integer) is
   begin
      mem.value := value;
   end Set_Value;

   function Get_Bank(mem : Transform_Type) return Memory_Pointer is
   begin
      return Memory_Pointer(mem.bank);
   end Get_Bank;

   procedure Set_Bank(mem  : in out Transform_Type;
                      bank : access Memory_Type'Class) is
   begin
      mem.bank := bank;
   end Set_Bank;

   procedure Reset(mem : in out Transform_Type) is
   begin
      Reset(Container_Type(mem));
      if mem.bank /= null then
         Reset(mem.bank.all);
      end if;
   end Reset;

   procedure Read(mem      : in out Transform_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      start : Time_Type;
   begin
      if mem.bank /= null then
         start := Get_Time(mem.bank.all);
         Process(mem, address, Address_Type(size), True, True);
         Advance(mem, Get_Time(mem.bank.all) - start);
      else
         Process(mem, address, Address_Type(size), True, True);
      end if;
   end Read;

   procedure Write(mem     : in out Transform_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      start : Time_Type;
   begin
      if mem.bank /= null then
         start := Get_Time(mem.bank.all);
         Process(mem, address, Address_Type(size), True, False);
         Advance(mem, Get_Time(mem.bank.all) - start);
      else
         Process(mem, address, Address_Type(size), True, False);
      end if;
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
      if mem.bank /= null then
         result := result + Get_Cost(mem.bank.all);
      end if;
      return result;
   end Get_Cost;

   function Is_Empty(mem : Transform_Type) return Boolean is
   begin
      return mem.bank /= null and then mem.bank.all in Join_Type'Class;
   end Is_Empty;

   function Get_Alignment(mem : Transform_Type) return Positive is
   begin
      return 1;
   end Get_Alignment;

   procedure Generate(mem  : in Transform_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
   begin
      if mem.bank /= null then
         Generate_Banked(Transform_Type'Class(mem), sigs, code);
      else
         Generate_Simple(Transform_Type'Class(mem), sigs, code);
      end if;
   end Generate;

   procedure Adjust(mem : in out Transform_Type) is
      jp    : Join_Pointer;
      cp    : Container_Pointer;
      ptr   : Memory_Pointer;
   begin
      Adjust(Container_Type(mem));
      if mem.bank /= null then
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
      end if;
   end Adjust;

   procedure Finalize(mem : in out Transform_Type) is
   begin
      Finalize(Container_Type(mem));
      Destroy(Memory_Pointer(mem.bank));
   end Finalize;

end Memory.Transform;
