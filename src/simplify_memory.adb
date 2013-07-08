
with Memory.Container; use Memory.Container;
with Memory.Transform; use Memory.Transform;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Split; use Memory.Split;
with Memory.Join; use Memory.Join;

function Simplify_Memory(mem : Memory_Pointer) return Memory_Pointer is

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
         Set_Memory(tp.all, null);
         Destroy(Memory_Pointer(tp));
         return next;
      else
         Set_Memory(tp.all, next);
         return Memory_Pointer(ptr);
      end if;

   end Simplify_Transform;

begin
   if mem.all in Split_Type'Class then
      declare
         sp : Split_Pointer   := Split_Pointer(mem);
         b0 : Memory_Pointer  := Get_Bank(sp.all, 0);
         b1 : Memory_Pointer  := Get_Bank(sp.all, 1);
         n  : Memory_Pointer  := Get_Memory(sp.all);
      begin
         b0 := Simplify_Memory(b0);
         Set_Bank(sp.all, 0, b0);
         b1 := Simplify_Memory(b1);
         Set_Bank(sp.all, 1, b1);
         n := Simplify_Memory(n);
         if b0.all in Join_Type'Class and b1.all in Join_Type'Class then
            Set_Memory(sp.all, null);
            Destroy(Memory_Pointer(sp));
            return n;
         else
            Set_Memory(sp.all, n);
            return mem;
         end if;
      end;
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
