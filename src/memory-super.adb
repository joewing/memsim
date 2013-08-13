
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Assertions;          use Ada.Assertions;
with Ada.Unchecked_Deallocation;
with Memory.Cache;            use Memory.Cache;
with Memory.SPM;              use Memory.SPM;
with Memory.Transform;        use Memory.Transform;
with Memory.Transform.Flip;   use Memory.Transform.Flip;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Transform.Shift;  use Memory.Transform.Shift;
with Memory.Transform.EOR;    use Memory.Transform.EOR;
with Memory.Prefetch;         use Memory.Prefetch;
with Memory.Split;            use Memory.Split;
with Memory.Join;             use Memory.Join;
with Memory.Register;         use Memory.Register;
with Memory.Option;           use Memory.Option;
with Applicative;             use Applicative;
with Simplify_Memory;

package body Memory.Super is

   function Create_Memory(mem       : Super_Type;
                          next      : access Memory_Type'Class;
                          cost      : Cost_Type;
                          in_bank   : Boolean)
                          return Memory_Pointer;

   function Create_Split(mem     : Super_Type;
                         next    : access Memory_Type'Class;
                         cost    : Cost_Type;
                         in_bank : Boolean)
                         return Memory_Pointer;

   function Create_Transform(mem       : Super_Type;
                             next      : access Memory_Type'Class;
                             cost      : Cost_Type;
                             in_bank   : Boolean)
                             return Memory_Pointer;

   function Create_Memory(mem       : Super_Type;
                          next      : access Memory_Type'Class;
                          cost      : Cost_Type;
                          in_bank   : Boolean)
                          return Memory_Pointer is
      tcost    : constant Cost_Type := cost + Get_Cost(next.all);
      result   : Memory_Pointer := null;
   begin
      case Random(mem.generator.all) mod 8 is
         when 0      =>
            result := Create_Transform(mem, next, cost, in_bank);
         when 1      =>
            if mem.has_idle then
               result := Random_Prefetch(next, mem.generator.all, tcost);
            else
               result := Create_Memory(mem, next, cost, in_bank);
            end if;
         when 2      =>
            result := Random_SPM(next, mem.generator.all, tcost);
         when 3      =>
            result := Random_Cache(next, mem.generator.all, tcost);
         when 4      =>
            result := Create_Split(mem, next, cost, in_bank);
         when others =>
            result := Create_Memory(mem, next, cost, in_bank);
      end case;
      Assert(result /= null, "Memory.Super.Create_Memory returning null");
      return result;
   end Create_Memory;

   function Create_Split(mem     : Super_Type;
                         next    : access Memory_Type'Class;
                         cost    : Cost_Type;
                         in_bank : Boolean)
                         return Memory_Pointer is
      result   : Split_Pointer;
      join0    : Join_Pointer;
      join1    : Join_Pointer;
   begin
      result   := Split_Pointer(Random_Split(next, mem.generator.all, cost));
      join0    := Create_Join(result, 0);
      join1    := Create_Join(result, 1);
      Set_Bank(result.all, 0, join0);
      Set_Bank(result.all, 1, join1);
      return Memory_Pointer(result);
   end Create_Split;

   function Create_Transform(mem       : Super_Type;
                             next      : access Memory_Type'Class;
                             cost      : Cost_Type;
                             in_bank   : Boolean)
                             return Memory_Pointer is
      result   : Memory_Pointer;
      trans    : Transform_Pointer;
      join     : Join_Pointer;
   begin
      case Random(mem.generator.all) mod 4 is
         when 0 =>
            result := Random_Flip(next, mem.generator.all, cost);
         when 1 =>
            result := Random_Offset(next, mem.generator.all, cost);
         when 2 =>
            result := Random_EOR(next, mem.generator.all, cost);
         when others =>
            result := Random_Shift(next, mem.generator.all, cost);
      end case;
      trans := Transform_Pointer(result);
      if in_bank or else (Random(mem.generator.all) mod 2) = 0 then
         join := Create_Join(trans, 0);
         Set_Bank(trans.all, join);
      end if;
      return result;
   end Create_Transform;

   function Clone(mem : Super_Type) return Memory_Pointer is
      result : constant Super_Pointer := new Super_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   function Done(mem : Super_Type) return Boolean is
   begin
      return mem.iteration >= mem.max_iterations;
   end Done;

   function Count_Memories(ptr : Memory_Pointer) return Natural is
   begin
      if ptr /= null then
         if ptr.all in Split_Type'Class then
            declare
               sp : constant Split_Pointer   := Split_Pointer(ptr);
               a  : constant Memory_Pointer  := Get_Bank(sp.all, 0);
               ac : constant Natural         := Count_Memories(a);
               b  : constant Memory_Pointer  := Get_Bank(sp.all, 1);
               bc : constant Natural         := Count_Memories(b);
               n  : constant Memory_Pointer  := Get_Memory(sp.all);
               nc : constant Natural         := Count_Memories(n);
            begin
               return 1 + ac + bc + nc;
            end;
         elsif ptr.all in Transform_Type'Class then
            declare
               tp : constant Transform_Pointer  := Transform_Pointer(ptr);
               bp : constant Memory_Pointer     := Get_Bank(tp.all);
               bc : constant Natural            := Count_Memories(bp);
               np : constant Memory_Pointer     := Get_Memory(tp.all);
               nc : constant Natural            := Count_Memories(np);
            begin
               return 1 + bc + nc;
            end;
         elsif ptr.all in Container_Type'Class then
            declare
               cp : constant Container_Pointer := Container_Pointer(ptr);
            begin
               return 1 + Count_Memories(Get_Memory(cp.all));
            end;
         elsif ptr.all in Join_Type'Class then
            return 1;
         elsif ptr.all in Option_Type'Class then
            return 1;
         end if;
      end if;
      return 0;
   end Count_Memories;

   procedure Reset(mem : in out Super_Type) is
   begin
      mem.current_length := 0;
      Reset(Container_Type(mem));
   end Reset;

   procedure Read(mem      : in out Super_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      mem.current_length := mem.current_length + 1;
      Read(Container_Type(mem), address, size);
      if mem.total = 0 then
         Insert(mem.generator.all, address, size);
      elsif Get_Value(mem'Access) >= mem.best_value * 2 then
         if mem.current_length > mem.total_length / 4 then
            raise Prune_Error;
         end if;
      end if;
   end Read;

   procedure Write(mem     : in out Super_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      mem.current_length := mem.current_length + 1;
      Write(Container_Type(mem), address, size);
      if mem.total = 0 then
         Insert(mem.generator.all, address, size);
      elsif Get_Value(mem'Access) >= mem.best_value * 2 then
         if mem.current_length > mem.total_length / 4 then
            raise Prune_Error;
         end if;
      end if;
   end Write;

   procedure Idle(mem      : in out Super_Type;
                  cycles   : in Time_Type) is
   begin
      if cycles > 0 then
         mem.has_idle := True;
         Idle(Container_Type(mem), cycles);
      end if;
   end Idle;

   procedure Remove_Memory(ptr      : in out Memory_Pointer;
                           index    : in Natural;
                           updated  : out Boolean) is
   begin

      Assert(ptr /= null, "null ptr in Remove_Memory");

      if index = 0 then
         if ptr.all in Split_Type'Class then
            declare
               sp    : constant Split_Pointer   := Split_Pointer(ptr);
               b0    : constant Memory_Pointer  := Get_Bank(sp.all, 0);
               b1    : constant Memory_Pointer  := Get_Bank(sp.all, 1);
               next  : constant Memory_Pointer  := Get_Memory(sp.all);
            begin
               if b0.all in Join_Type'Class
                  and b1.all in Join_Type'Class then
                  Set_Memory(sp.all, null);
                  Destroy(ptr);
                  ptr := next;
                  updated := True;
               else
                  updated := False;
               end if;
            end;
         elsif ptr.all in Container_Type'Class then
            declare
               cp    : constant Container_Pointer  := Container_Pointer(ptr);
               next  : constant Memory_Pointer     := Get_Memory(cp.all);
            begin
               Set_Memory(cp.all, null);
               Destroy(ptr);
               ptr := next;
               updated := True;
            end;
         elsif ptr.all in Join_Type'Class then
            updated := False;
         elsif ptr.all in Option_Type'Class then
            updated := False;
         else
            Destroy(ptr);
            ptr := null;
            updated := True;
         end if;
      elsif ptr.all in Split_Type'Class then
         declare
            sp : constant Split_Pointer   := Split_Pointer(ptr);
            a  : Memory_Pointer           := Get_Bank(sp.all, 0);
            ac : constant Natural         := Count_Memories(a);
            b  : Memory_Pointer           := Get_Bank(sp.all, 1);
            bc : constant Natural         := Count_Memories(b);
            n  : Memory_Pointer           := Get_Memory(sp.all);
         begin
            if 1 + ac > index then
               Remove_Memory(a, index - 1, updated);
               Set_Bank(sp.all, 0, a);
            elsif 1 + ac + bc > index then
               Remove_Memory(b, index - ac - 1, updated);
               Set_Bank(sp.all, 1, b);
            else
               Remove_Memory(n, index - ac - bc - 1, updated);
               Set_Memory(sp.all, n);
            end if;
         end;
      elsif ptr.all in Transform_Type'Class then
         declare
            tp    : constant Transform_Pointer  := Transform_Pointer(ptr);
            bank  : Memory_Pointer              := Get_Bank(tp.all);
            count : constant Natural            := Count_Memories(bank);
            next  : Memory_Pointer              := Get_Memory(tp.all);
         begin
            if 1 + count > index then
               Remove_Memory(bank, index - 1, updated);
               Set_Bank(tp.all, bank);
            else
               Remove_Memory(next, index - count - 1, updated);
               Set_Memory(tp.all, next);
            end if;
         end;
      elsif ptr.all in Container_Type'Class then
         declare
            cp : constant Container_Pointer  := Container_Pointer(ptr);
            n  : Memory_Pointer              := Get_Memory(cp.all);
         begin
            Remove_Memory(n, index - 1, updated);
            Set_Memory(cp.all, n);
         end;
      else
         updated := False;
      end if;
   end Remove_Memory;

   procedure Insert_Memory(mem      : in Super_Type;
                           ptr      : in out Memory_Pointer;
                           index    : in Natural;
                           cost     : in Cost_Type;
                           in_bank  : in Boolean) is
   begin

      Assert(ptr /= null, "null ptr in Insert_Memory");

      if index = 0 then

         -- Insert other before ptr.
         ptr := Create_Memory(mem, ptr, cost, in_bank);

      elsif ptr.all in Split_Type'Class then

         declare
            sp : constant Split_Pointer   := Split_Pointer(ptr);
            a  : Memory_Pointer           := Get_Bank(sp.all, 0);
            ac : constant Natural         := Count_Memories(a);
            b  : Memory_Pointer           := Get_Bank(sp.all, 1);
            bc : constant Natural         := Count_Memories(b);
            n  : Memory_Pointer           := Get_Memory(sp.all);
         begin
            if 1 + ac > index then

               -- Insert to the first bank.
               Push_Limit(mem.generator.all, 0, Get_Offset(sp.all));
               Insert_Memory(mem, a, index - 1, cost, True);
               Pop_Limit(mem.generator.all);
               Set_Bank(sp.all, 0, a);

            elsif 1 + ac + bc > index then

               -- Insert to the second bank.
               Push_Limit(mem.generator.all, Get_Offset(sp.all),
                          Address_Type'Last);
               Insert_Memory(mem, b, index - ac - 1, cost, True);
               Pop_Limit(mem.generator.all);
               Set_Bank(sp.all, 1, b);

            else

               -- Insert after the split.
               Insert_Memory(mem, n, index - ac - bc - 1, cost, in_bank);
               Set_Memory(sp.all, n);

            end if;
         end;

      elsif ptr.all in Transform_Type'Class then

         declare
            tp    : constant Transform_Pointer  := Transform_Pointer(ptr);
            bank  : Memory_Pointer              := Get_Bank(tp.all);
            count : constant Natural            := Count_Memories(bank);
            next  : Memory_Pointer              := Get_Memory(tp.all);
         begin
            if 1 + count > index then

               -- Insert to the bank.
               Push_Transform(mem.generator.all, Applicative_Pointer(tp));
               Insert_Memory(mem, bank, index - 1, cost, True);
               Pop_Transform(mem.generator.all);
               Set_Bank(tp.all, bank);

            else

               -- Insert after the transform.
               if Get_Bank(tp.all) = null then
                  Push_Transform(mem.generator.all, Applicative_Pointer(tp));
                  Insert_Memory(mem, next, index - count - 1, cost, in_bank);
                  Pop_Transform(mem.generator.all);
               else
                  Insert_Memory(mem, next, index - count - 1, cost, in_bank);
               end if;
               Set_Memory(tp.all, next);

            end if;
         end;

      elsif ptr.all in Container_Type'Class then

         declare
            cp : constant Container_Pointer  := Container_Pointer(ptr);
            n  : Memory_Pointer              := Get_Memory(cp.all);
         begin
            Insert_Memory(mem, n, index - 1, cost, in_bank);
            Set_Memory(cp.all, n);
         end;

      end if;

   end Insert_Memory;

   function Permute_Memory(mem   : in Super_Type;
                           ptr   : in Memory_Pointer;
                           index : in Natural;
                           cost  : in Cost_Type) return Boolean is
   begin

      Assert(ptr /= null, "null ptr in Permute_Memory");

      if index = 0 then

         Permute(ptr.all, mem.generator.all, cost + Get_Cost(ptr.all));
         return ptr.all not in Join_Type'Class;

      elsif ptr.all in Split_Type'Class then

         declare
            sp : constant Split_Pointer   := Split_Pointer(ptr);
            a  : constant Memory_Pointer  := Get_Bank(sp.all, 0);
            ac : constant Natural         := Count_Memories(a);
            b  : constant Memory_Pointer  := Get_Bank(sp.all, 1);
            bc : constant Natural         := Count_Memories(b);
            n  : constant Memory_Pointer  := Get_Memory(sp.all);
            rc : Boolean;
         begin
            if 1 + ac > index then
               Push_Limit(mem.generator.all, 0, Get_Offset(sp.all));
               rc := Permute_Memory(mem, a, index - 1, cost);
               Pop_Limit(mem.generator.all);
            elsif 1 + ac + bc > index then
               Push_Limit(mem.generator.all, Get_Offset(sp.all),
                          Address_Type'Last);
               rc := Permute_Memory(mem, b, index - ac - 1, cost);
               Pop_Limit(mem.generator.all);
            else
               rc := Permute_Memory(mem, n, index - ac - bc - 1, cost);
            end if;
            return rc;
         end;

      elsif ptr.all in Transform_Type'Class then

         declare
            tp    : constant Transform_Pointer  := Transform_Pointer(ptr);
            bank  : constant Memory_Pointer     := Get_Bank(tp.all);
            count : constant Natural            := Count_Memories(bank);
            next  : constant Memory_Pointer     := Get_Memory(tp.all);
            rc    : Boolean;
         begin
            if 1 + count > index then
               Push_Transform(mem.generator.all, Applicative_Pointer(tp));
               rc := Permute_Memory(mem, bank, index - 1, cost);
               Pop_Transform(mem.generator.all);
            else
               if Get_Bank(tp.all) = null then
                  Push_Transform(mem.generator.all, Applicative_Pointer(tp));
                  rc := Permute_Memory(mem, next, index - count - 1, cost);
                  Pop_Transform(mem.generator.all);
               else
                  rc := Permute_Memory(mem, next, index - count - 1, cost);
               end if;
            end if;
            return rc;
         end;

      elsif ptr.all in Container_Type'Class then

         declare
            cp : constant Container_Pointer  := Container_Pointer(ptr);
            n  : constant Memory_Pointer     := Get_Memory(cp.all);
         begin
            return Permute_Memory(mem, n, index - 1, cost);
         end;

      else

         Assert(False, "invalid type in Permute_Memory");
         return False;

      end if;
   end Permute_Memory;

   procedure Randomize(mem : in Super_Type; ptr : in out Memory_Pointer) is
      len   : constant Natural := Count_Memories(ptr);
      pos   : Natural;
      rc    : Boolean;
      cost  : constant Cost_Type := Get_Cost(ptr.all);
      left  : constant Cost_Type := mem.max_cost - cost;
   begin

      -- Select an action to take.
      -- Here we give extra weight to modifying an existing subsystem
      -- rather than adding or removing components.
      case Random(mem.generator.all) mod 32 is
         when 0      =>    -- Insert a component.
            pos := Random(mem.generator.all) mod (len + 1);
            Insert_Memory(mem, ptr, pos, left, False);
         when 1 | 2  =>    -- Remove a component.
            if len = 0 then
               Insert_Memory(mem, ptr, 0, left, False);
            else
               for i in 1 .. 10 loop
                  pos := Random(mem.generator.all) mod len;
                  Remove_Memory(ptr, pos, rc);
                  exit when rc;
               end loop;
            end if;
         when others =>    -- Modify a component.
            if len = 0 then
               Insert_Memory(mem, ptr, 0, left, False);
            else
               loop
                  pos := Random(mem.generator.all) mod len;
                  exit when Permute_Memory(mem, ptr, pos, left);
               end loop;
            end if;
      end case;

      Assert(Get_Cost(mem) <= mem.max_cost, "Invalid randomize");

   end Randomize;

   function Create_Super(mem              : not null access Memory_Type'Class;
                         max_cost         : Cost_Type;
                         seed             : Integer;
                         max_iterations   : Long_Integer)
                         return Super_Pointer is
      result : constant Super_Pointer := new Super_Type;
   begin
      result.max_cost         := max_cost;
      result.max_iterations   := max_iterations;
      result.last             := Memory_Pointer(mem);
      result.current          := Clone(mem.all);
      result.generator        := new Distribution_Type;
      Set_Seed(result.generator.all, seed);
      Set_Memory(result.all, Clone(mem.all));
      return result;
   end Create_Super;

   procedure Update_Memory(mem   : in out Super_Type;
                           value : in Value_Type) is
      eold  : constant Long_Integer := Long_Integer(mem.last_value);
      enew  : constant Long_Integer := Long_Integer(value);
      diff  : constant Long_Integer := enew - eold;
      temp  : Memory_Pointer;
   begin

      if mem.steps = 0 then
         mem.threshold := enew / 2;
      end if;

      if diff <= mem.threshold then

         -- Keep the current memory.
         mem.last_value := value;
         Destroy(mem.last);
         mem.last := Clone(mem.current.all);

         -- Decrease the threshold.
         mem.threshold := mem.threshold - (mem.threshold + 1023) / 1024;

      else

         -- Revert to the previous memory.
         Destroy(mem.current);
         mem.current := Clone(mem.last.all);

         -- Increase the threshold.
         mem.threshold := mem.threshold + mem.threshold / 1024 + 1;

      end if;

      mem.steps := mem.steps + 1;

      temp := Get_Memory(mem);
      Destroy(temp);
      Set_Memory(mem, null);

      Randomize(mem, mem.current);
      temp := Simplify_Memory(Clone(mem.current.all));
      Insert_Registers(temp);
      Set_Memory(mem, temp);

   end Update_Memory;

   procedure Track_Best(mem   : in out Super_Type;
                        cost  : in Cost_Type;
                        value : in Value_Type) is
      simp_mem    : Memory_Pointer;
      simp_cost   : Cost_Type;
      simp_name   : Unbounded_String;
   begin

      -- Value can't change from simplification, so we check it first.
      if value > mem.best_value then
         return;
      end if;

      -- Value is at least as good.
      -- The current memory being used is the simplified memory with
      -- registers inserted, so we use that.
      simp_mem    := Get_Memory(mem);
      simp_cost   := Get_Cost(simp_mem.all);

      -- If the value is the same, we will only accept the memory if
      -- the cost is at least as good.
      if value = mem.best_value and cost > mem.best_cost then
         return;
      end if;

      -- Get the name of the simplified memory.
      simp_name := To_String(simp_mem.all);

      -- If the cost and value are the same, we accept the memory
      -- only if the name is shorter.
      if value = mem.best_value and then cost = mem.best_cost and then
         Length(simp_name) > Length(mem.best_name) then
         return;
      end if;

      -- If we get here, we have a better memory subsystem.
      if mem.best_value < Value_Type'Last then

         -- Don't reset our count unless we are able to get at least
         -- 0.1% better since the last reset.
         mem.improvement   := mem.improvement + mem.best_value - value;
         if mem.improvement > value / 1000 then
            mem.iteration     := 0;
            mem.improvement   := 0;
         end if;

      end if;
      mem.best_value    := value;
      mem.best_cost     := simp_cost;
      mem.best_name     := simp_name;

   end Track_Best;

   procedure Cache_Result(mem    : in out Super_Type;
                          value  : in Value_Type) is
      simp_mem    : Memory_Pointer;
      simp_name   : Unbounded_String;
   begin
      simp_mem    := Simplify_Memory(Clone(mem.current.all));
      simp_name   := To_String(simp_mem.all);
      mem.table.Insert(simp_name, value);
      Destroy(simp_mem);
   end Cache_Result;

   function Check_Cache(mem : Super_Type) return Value_Type is
      simp_mem    : Memory_Pointer;
      simp_name   : Unbounded_String;
      cursor      : Value_Maps.Cursor;
   begin
      simp_mem    := Simplify_Memory(Clone(mem.current.all));
      simp_name   := To_String(simp_mem.all);
      Destroy(simp_mem);
      cursor      := mem.table.Find(simp_name);
      if Value_Maps."="(cursor, Value_Maps.No_Element) then
         return 0;
      else
         return Value_Maps.Element(cursor);
      end if;
   end Check_Cache;

   procedure Finish_Run(mem : in out Super_Type) is
      cost  : constant Cost_Type := Get_Cost(mem);
      value : Value_Type := Get_Value(mem'Access);
   begin

      -- Scale the result if necessary.
      if mem.current_length > mem.total_length then
         mem.total_length := mem.current_length;
      end if;
      if mem.current_length /= mem.total_length then
         Put_Line("Prune");
         declare
            mult : constant Long_Float := Long_Float(mem.total_length) *
                                          Long_Float(mem.current_length);
            fval : constant Long_Float := Long_Float(value) * mult;
         begin
            if fval >= Long_Float(Value_Type'Last) then
               value := Value_Type'Last;
            else
               value := Value_Type(fval);
            end if;
         end;
         if value < mem.best_value then
            Put_Line("Prune overflow");
            value := mem.best_value + 1;
         end if;
      end if;

      -- Keep track of the best memory.
      Track_Best(mem, cost, value);
      Put_Line("Best Memory: " & To_String(mem.best_name));
      Put_Line("Best Value:  " & Value_Type'Image(mem.best_value));
      Put_Line("Best Cost:   " & Cost_Type'Image(mem.best_cost));

      -- Keep track of the result of running with this memory.
      Cache_Result(mem, value);

      -- Keep track of the number of iterations.
      mem.iteration  := mem.iteration + 1;
      mem.total      := mem.total + 1;
      if not Done(mem) then

         Put_Line("Iteration:" & Long_Integer'Image(mem.iteration + 1) &
                  " (evaluation " & To_String(mem.total + 1) &
                  ", steps " & To_String(mem.steps + 1) &
                  ", threshold " & To_String(mem.threshold) & ")");

         -- Generate new memories until we find a new one.
         loop
            Update_Memory(mem, value);
            value := Check_Cache(mem);
            exit when value = 0;
         end loop;
         Put_Line(To_String(To_String(mem)));

      end if;

   end Finish_Run;

   procedure Show_Access_Stats(mem : in out Super_Type) is
   begin
      Finish_Run(mem);
   end Show_Access_Stats;

   procedure Adjust(mem : in out Super_Type) is
   begin
      Adjust(Container_Type(mem));
      mem.generator  := new Distribution_Type;
      mem.last       := null;
      mem.current    := null;
   end Adjust;

   procedure Destroy is new Ada.Unchecked_Deallocation(Distribution_Type,
                                                       Distribution_Pointer);

   procedure Finalize(mem : in out Super_Type) is
   begin
      Destroy(mem.generator);
      Destroy(mem.last);
      Destroy(mem.current);
      Finalize(Container_Type(mem));
   end Finalize;

end Memory.Super;
