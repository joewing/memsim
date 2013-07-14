
with Memory.Container; use Memory.Container;
with Memory.Transform; use Memory.Transform;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Split; use Memory.Split;
with Memory.Join; use Memory.Join;
with Memory.SPM; use Memory.SPM;
with Util; use Util;

function Simplify_Memory(mem : Memory_Pointer) return Memory_Pointer is

   function Needs_Split(ptr : Memory_Pointer) return Boolean is
      tp : Memory_Pointer := ptr;
   begin
      while tp.all not in Join_Type'Class loop
         if tp.all not in SPM_Type'Class then
            return True;
         end if;
         declare
            cp : constant Container_Pointer := Container_Pointer(tp);
         begin
            tp := Get_Memory(cp.all);
         end;
      end loop;
      return False;
   end Needs_Split;

   procedure Replace_Join(bank : in Memory_Pointer;
                          next : in Memory_Pointer) is
      last  : Container_Pointer := null;
      tp    : Memory_Pointer := bank;
   begin
      while tp.all not in Join_Type'Class loop
         last := Container_Pointer(tp);
         tp := Get_Memory(last.all);
      end loop;
      Set_Memory(last.all, next);
      Destroy(tp);
   end Replace_Join;

   function Simplify_Split(ptr : Split_Pointer) return Memory_Pointer is
      sp    : Split_Pointer   := ptr;
      b0    : Memory_Pointer  := Get_Bank(sp.all, 0);
      b1    : Memory_Pointer  := Get_Bank(sp.all, 1);
      n     : Memory_Pointer  := Get_Memory(sp.all);
      wsize : constant Address_Type := Address_Type(Get_Word_Size(sp.all));
   begin
      b0 := Simplify_Memory(b0);
      Set_Bank(sp.all, 0, b0);
      b1 := Simplify_Memory(b1);
      Set_Bank(sp.all, 1, b1);
      n := Simplify_Memory(n);
      if b0.all in Join_Type'Class and b1.all in Join_Type'Class then
         -- Empty split; remove it.
         Set_Memory(sp.all, null);
         Destroy(Memory_Pointer(sp));
         return n;
      elsif b0.all in Join_Type'Class and not Needs_Split(b1) then
         -- Split used as an offset; convert to an offset with a bank.
         declare
            op : constant Offset_Pointer  := Create_Offset;
            jp : constant Join_Pointer    := Find_Join(b1);
         begin
            Set_Bank(sp.all, 1, null);
            Set_Memory(sp.all, null);
            Set_Parent(jp.all, op);
            Set_Bank(op.all, b1);
            Set_Memory(op.all, n);
            Set_Value(op.all, Long_Integer(Get_Offset(sp.all) / wsize));
            return Memory_Pointer(op);
         end;
      elsif b1.all in Join_Type'Class and not Needs_Split(b0) then
         -- Split not needed; remove it.
         Set_Bank(sp.all, 0, null);
         Set_Memory(sp.all, null);
         Destroy(Memory_Pointer(sp));
         Replace_Join(b0, n);
         return b0;
      else
         Set_Memory(sp.all, n);
         return Memory_Pointer(ptr);
      end if;
   end Simplify_Split;

   function Simplify_Transform(ptr : Transform_Pointer)
                               return Memory_Pointer is
         tp    : Transform_Pointer  := ptr;
         bank  : Memory_Pointer     := Get_Bank(tp.all);
         next  : Memory_Pointer     := Get_Memory(tp.all);
   begin

      next := Simplify_Memory(next);
      if bank /= null then
         bank := Simplify_Memory(bank);
         Set_Bank(tp.all, bank);
      end if;

      if bank = null and next.all in Offset_Type'Class then
         declare
            nt : Transform_Pointer        := Transform_Pointer(next);
            oo : constant Long_Integer    := Get_Value(nt.all);
         begin
            Set_Value(tp.all, Get_Value(tp.all) + oo);
            next := Get_Memory(nt.all);
            Set_Memory(nt.all, null);
            Destroy(Memory_Pointer(nt));
         end;
      end if;

      if tp.Is_Empty then
         if bank = null or else bank.all in Join_Type'Class then
            Set_Memory(tp.all, null);
            Destroy(Memory_Pointer(tp));
            return next;
         else
            Set_Memory(tp.all, null);
            Set_Bank(tp.all, null);
            Destroy(Memory_Pointer(tp));
            Replace_Join(bank, next);
            return bank;
         end if;
      else
         Set_Memory(tp.all, next);
         return Memory_Pointer(ptr);
      end if;

   end Simplify_Transform;

begin
   if mem.all in Split_Type'Class then
      return Simplify_Split(Split_Pointer(mem));
   elsif mem.all in Transform_Type'Class then
      return Simplify_Transform(Transform_Pointer(mem));
   elsif mem.all in Container_Type'Class then
      declare
         cp : constant Container_Pointer  := Container_Pointer(mem);
         n  : Memory_Pointer              := Get_Memory(cp.all);
      begin
         n := Simplify_Memory(n);
         Set_Memory(cp.all, n);
         return mem;
      end;
   else
      return mem;
   end if;
end Simplify_Memory;
