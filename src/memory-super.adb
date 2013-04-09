
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Assertions;          use Ada.Assertions;
with Ada.Numerics.Generic_Elementary_Functions;
with Memory.Cache;            use Memory.Cache;
with Memory.SPM;              use Memory.SPM;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Transform.Shift;  use Memory.Transform.Shift;
with Memory.Prefetch;         use Memory.Prefetch;
with Memory.Split;            use Memory.Split;
with Memory.Join;             use Memory.Join;

package body Memory.Super is

   package Float_Math is new Ada.Numerics.Generic_Elementary_Functions(Float);

   function Create_Memory(mem       : Super_Type;
                          cost      : Cost_Type;
                          in_split  : Boolean)
                          return Memory_Pointer;

   function Create_Split(mem        : Super_Type;
                         cost       : Cost_Type)
                         return Memory_Pointer;

   function Create_Memory(mem       : Super_Type;
                          cost      : Cost_Type;
                          in_split  : Boolean)
                          return Memory_Pointer is
      result : Memory_Pointer := null;
   begin
      if in_split then
         case RNG.Random(mem.generator.all) mod 4 is
            when 0      =>
               result := Random_Prefetch(mem.generator.all, cost);
            when 1      =>
               result := Random_SPM(mem.generator.all, cost);
            when 2      =>
               result := Random_Cache(mem.generator.all, cost);
            when others =>
               result := Create_Split(mem, cost);
         end case;
      else
         case RNG.Random(mem.generator.all) mod 6 is
            when 0      =>
               result := Random_Shift(mem.generator.all, cost);
            when 1      =>
               result := Random_Offset(mem.generator.all, cost);
            when 2      =>
               result := Random_Prefetch(mem.generator.all, cost);
            when 3      =>
               result := Random_SPM(mem.generator.all, cost);
            when 4      =>
               result := Random_Cache(mem.generator.all, cost);
            when others =>
               result := Create_Split(mem, cost);
         end case;
      end if;
      return result;
   end Create_Memory;

   function Create_Split(mem        : Super_Type;
                         cost       : Cost_Type)
                         return Memory_Pointer is
      result   : constant Split_Pointer
                  := Split_Pointer(Random_Split(mem.generator.all, cost));
      bank0    : constant Memory_Pointer
                  := Create_Memory(mem, (cost + 1) / 2, True);
      bank1    : constant Memory_Pointer
                  := Create_Memory(mem, cost / 2, True);
   begin
      if bank0 /= null and then bank0.all in Container_Type'Class then
         Set_Memory(Container_Pointer(bank0).all, Create_Join);
         Set_Bank(result, 0, bank0);
      else
         Set_Bank(result, 0, Create_Join);
      end if;
      if bank1 /= null and then bank1.all in Container_Type'Class then
         Set_Memory(Container_Pointer(bank1).all, Create_Join);
         Set_Bank(result, 1, bank1);
      else
         Set_Bank(result, 1, Create_Join);
      end if;
      return Memory_Pointer(result);
   end Create_Split;

   function Clone(mem : Super_Type) return Memory_Pointer is
      result : constant Super_Pointer := new Super_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   function Count_Memories(ptr : Memory_Pointer) return Natural is
   begin
      if ptr /= null then
         if ptr.all in Split_Type'Class then
            declare
               sp : constant Split_Pointer   := Split_Pointer(ptr);
               a  : constant Memory_Pointer  := Get_Bank(sp, 0);
               ac : constant Natural         := Count_Memories(a);
               b  : constant Memory_Pointer  := Get_Bank(sp, 1);
               bc : constant Natural         := Count_Memories(b);
               n  : constant Memory_Pointer  := Get_Memory(sp.all);
               nc : constant Natural         := Count_Memories(n);
            begin
               return 1 + ac + bc + nc;
            end;
         elsif ptr.all in Container_Type'Class then
            declare
               cp : constant Container_Pointer := Container_Pointer(ptr);
            begin
               return 1 + Count_Memories(Get_Memory(cp.all));
            end;
         end if;
      end if;
      return 0;
   end Count_Memories;

   function Get_Memory(ptr    : Memory_Pointer;
                       index  : Natural) return Container_Pointer is

      temp : Natural := index;

   begin
      Assert(ptr /= null, "null ptr in Get_Memory");
      if ptr.all in Join_Type'Class then
         return null;
      elsif temp = 0 then
         return Container_Pointer(ptr);
      else
         temp := temp - 1;
         if ptr.all in Split_Type'Class then
            declare
               sp : constant Split_Pointer      := Split_Pointer(ptr);
               a  : constant Memory_Pointer     := Get_Bank(sp, 0);
               b  : constant Memory_Pointer     := Get_Bank(sp, 1);
               n  : constant Memory_Pointer     := Get_Memory(sp.all);
               r  : Container_Pointer;
            begin
               r := Get_Memory(a, temp);
               if r /= null then
                  return r;
               end if;
               temp := temp - Count_Memories(a);
               r := Get_Memory(b, temp);
               if r /= null then
                  return r;
               end if;
               temp := temp - Count_Memories(b);
               return Get_Memory(n, temp);
            end;
         elsif ptr.all in Container_Type'Class then
            declare
               cp : constant Container_Pointer  := Container_Pointer(ptr);
               n  : constant Memory_Pointer     := Get_Memory(cp.all);
            begin
               return Get_Memory(n, temp);
            end;
         end if;
      end if;
      return null;
   end Get_Memory;

   procedure Remove_Memory(ptr   : in out Memory_Pointer;
                           index : in Natural) is
   begin

      Assert(ptr /= null, "null ptr in Remove_Memory");

      if index = 0 then

         -- Remove this memory.
         if ptr.all in Container_Type'Class then
            declare
               cp    : constant Container_Pointer  := Container_Pointer(ptr);
               next  : constant Memory_Pointer     := Get_Memory(cp.all);
            begin
               Set_Memory(cp.all, null);
               Destroy(ptr);
               ptr := next;
            end;
         else
            Destroy(ptr);
            ptr := null;
         end if;

      elsif ptr.all in Split_Type'Class then
         declare
            sp : constant Split_Pointer   := Split_Pointer(ptr);
            a  : Memory_Pointer           := Get_Bank(sp, 0);
            ac : constant Natural         := Count_Memories(a);
            b  : Memory_Pointer           := Get_Bank(sp, 1);
            bc : constant Natural         := Count_Memories(b);
            n  : Memory_Pointer           := Get_Memory(sp.all);
         begin

            if 1 + ac > index then

               -- The memory to remove is in the first bank.
               Remove_Memory(a, index - 1);
               Set_Bank(sp, 0, a);

            elsif 1 + ac + bc > index then

               -- The memory to remove is in the second bank.
               Remove_Memory(b, index - ac - 1);
               Set_Bank(sp, 1, b);

            else

               -- The memory to remove follows this memory.
               Remove_Memory(n, index - ac - bc - 1);
               Set_Memory(sp.all, n);

            end if;
         end;
      elsif ptr.all in Container_Type'Class then
         declare
            cp : constant Container_Pointer  := Container_Pointer(ptr);
            n  : Memory_Pointer              := Get_Memory(cp.all);
         begin
            Remove_Memory(n, index - 1);
            Set_Memory(cp.all, n);
         end;
      end if;
   end Remove_Memory;

   procedure Insert_Memory(mem      : in Super_Type;
                           ptr      : in out Memory_Pointer;
                           index    : in Natural;
                           cost     : in Cost_Type;
                           in_split : in Boolean) is
   begin

      if index = 0 then

         -- Insert other before ptr.
         declare
            temp : constant Memory_Pointer :=
                   Create_Memory(mem, cost, in_split);
         begin
            if temp /= null then
               Set_Memory(Container_Pointer(temp).all, ptr);
               ptr := temp;
            end if;
         end;

      elsif ptr.all in Split_Type'Class then

         declare
            sp : constant Split_Pointer   := Split_Pointer(ptr);
            a  : Memory_Pointer           := Get_Bank(sp, 0);
            ac : constant Natural         := Count_Memories(a);
            b  : Memory_Pointer           := Get_Bank(sp, 1);
            bc : constant Natural         := Count_Memories(b);
            n  : Memory_Pointer           := Get_Memory(sp.all);
         begin

            if 1 + ac > index then

               -- Insert to the first bank.
               Insert_Memory(mem, a, index - 1, cost, True);
               Set_Bank(sp, 0, a);

            elsif 1 + ac + bc > index then

               -- Insert to the second bank.
               Insert_Memory(mem, b, index - ac - 1, cost, True);
               Set_Bank(sp, 1, b);

            else

               -- Insert after the split.
               Insert_Memory(mem, n, index - ac - bc - 1, cost, in_split);
               Set_Memory(sp.all, n);

            end if;

         end;

      elsif ptr.all in Container_Type'Class then
         declare
            cp : constant Container_Pointer  := Container_Pointer(ptr);
            n  : Memory_Pointer              := Get_Memory(cp.all);
         begin
            Insert_Memory(mem, n, index - 1, cost, in_split);
            Set_Memory(cp.all, n);
         end;
      end if;

   end Insert_Memory;

   function Simplify_Memory(ptr : Memory_Pointer) return Memory_Pointer is
   begin
      if ptr.all in Split_Type'Class then
         declare
            sp : Split_Pointer   := Split_Pointer(ptr);
            b0 : Memory_Pointer  := Get_Bank(sp, 0);
            b1 : Memory_Pointer  := Get_Bank(sp, 1);
            n  : Memory_Pointer  := Get_Memory(sp.all);
         begin
            b0 := Simplify_Memory(b0);
            Set_Bank(sp, 0, b0);
            b1 := Simplify_Memory(b1);
            Set_Bank(sp, 1, b1);
            n := Simplify_Memory(n);
            if b0.all in Join_Type'Class and b1.all in Join_Type'Class then
               Set_Memory(sp.all, null);
               Destroy(Memory_Pointer(sp));
               return n;
            else
               Set_Memory(sp.all, n);
               return ptr;
            end if;
         end;
      elsif ptr.all in Offset_Type'Class then
         declare
            op : Offset_Pointer  := Offset_Pointer(ptr);
            n  : Memory_Pointer  := Get_Memory(op.all);
         begin
            n := Simplify_Memory(n);
            if n.all in Offset_Type'Class then
               declare
                  nop : Offset_Pointer := Offset_Pointer(n);
               begin
                  Set_Offset(op.all, Get_Offset(op.all) + Get_Offset(nop.all));
                  n := Get_Memory(nop.all);
                  n := Simplify_Memory(n);
                  Set_Memory(nop.all, null);
                  Destroy(Memory_Pointer(nop));
               end;
            end if;
            if Get_Offset(op.all) = 0 then
               Set_Memory(op.all, null);
               Destroy(Memory_Pointer(op));
               return n;
            else
               Set_Memory(op.all, n);
               return Memory_Pointer(op);
            end if;
         end;
      elsif ptr.all in Shift_Type'Class then
         declare
            sp : Shift_Pointer   := Shift_Pointer(ptr);
            n  : Memory_Pointer  := Get_Memory(sp.all);
         begin
            n := Simplify_Memory(n);
            if Get_Shift(sp.all) = 0 then
               Set_Memory(sp.all, null);
               Destroy(Memory_Pointer(sp));
               return n;
            else
               Set_Memory(sp.all, n);
               return Memory_Pointer(sp);
            end if;
         end;
      elsif ptr.all in Container_Type'Class then
         declare
            cp : constant Container_Pointer  := Container_Pointer(ptr);
            n  : Memory_Pointer              := Get_Memory(cp.all);
         begin
            n := Simplify_Memory(n);
            Set_Memory(cp.all, n);
            return ptr;
         end;
      else
         return ptr;
      end if;
   end Simplify_Memory;

   procedure Randomize(mem : in out Super_Type) is
      temp  : Container_Pointer := null;
      len   : constant Natural := Count_Memories(mem.current);
      pos   : Natural;
      cost  : constant Cost_Type := Get_Cost(mem);
      left  : constant Cost_Type := mem.max_cost - cost;
   begin

      -- Select an action to take.
      -- Here we give extra weight to modifying an existing subsystem
      -- rather than adding or removing components.
      case RNG.Random(mem.generator.all) mod 16 is
         when 0      =>    -- Insert a component.
            pos := RNG.Random(mem.generator.all) mod (len + 1);
            Insert_Memory(mem, mem.current, pos, left, False);
         when 1 .. 3 =>    -- Remove a component.
            if len = 0 then
               Insert_Memory(mem, mem.current, 0, left, False);
            else
               pos := RNG.Random(mem.generator.all) mod len;
               Remove_Memory(mem.current, pos);
            end if;
         when others =>    -- Modify a component.
            if len = 0 then
               Insert_Memory(mem, mem.current, 0, left, False);
            else
               pos := RNG.Random(mem.generator.all) mod len;
               temp := Get_Memory(mem.current, pos);
               Permute(temp.all, mem.generator.all, left + Get_Cost(temp.all));
            end if;
      end case;
      mem.current := Simplify_Memory(mem.current);
      Set_Memory(mem, mem.current);

      Put_Line(To_String(To_String(mem)));
      Assert(Get_Cost(mem) <= mem.max_cost, "Invalid randomize");

   end Randomize;

   function Create_Super(mem        : not null access Memory_Type'Class;
                         max_cost   : Cost_Type;
                         seed       : Integer;
                         initial    : Long_Integer)
                         return Super_Pointer is
      result : constant Super_Pointer := new Super_Type;
   begin
      result.max_cost   := max_cost;
      result.initial    := initial;
      result.last       := Clone(mem.all);
      result.current    := Clone(mem.all);
      RNG.Reset(result.generator.all, seed);
      Set_Memory(result.all, result.current);
      Randomize(Super_Type(result.all));
      return result;
   end Create_Super;

   procedure Update_Memory(mem   : in out Super_Type;
                           value : in Value_Type) is
      eold  : constant Float := Float(mem.last_value);
      enew  : constant Float := Float(value);
      dele  : constant Float := abs(enew - eold);
      prob  : constant Float := Float_Math.Exp(-dele / mem.temperature);
      rand  : constant Float := Float(RNG.Random(mem.generator.all)) /
                                Float(Natural'Last);
   begin

      -- Update the temperature.
      mem.temperature := mem.temperature * 0.99;
      if mem.temperature < 1.0 / Float(Natural'Last) then
         mem.temperature := enew;
      end if;
      Put_Line("Temperature:" & Float'Image(mem.temperature));


      if value <= mem.last_value or rand < prob or mem.iteration = 0 then

         -- Keep the current memory.
         mem.last_value := value;
         Destroy(mem.last);
         mem.last := Clone(mem.current.all);

      else

         -- Revert to the previous memory.
         Destroy(mem.current);
         mem.current := Clone(mem.last.all);

      end if;

      -- Make a random modification to the memory.
      Set_Memory(mem, mem.current);
      Randomize(mem);

   end Update_Memory;

   procedure Finish_Run(mem : in out Super_Type) is
      name  : constant Unbounded_String := To_String(mem);
      cost  : constant Cost_Type := Get_Cost(mem);
      value : Value_Type := Get_Value(mem'Access);
   begin

      -- Keep track of the best memory.
      if value < mem.best_value or else
         (value = mem.best_value and cost < mem.best_cost) or else
         (value = mem.best_value and then cost = mem.best_cost and then
            Length(name) < Length(mem.best_name)) then
         mem.best_value := value;
         mem.best_cost  := cost;
         mem.best_name  := name;
      end if;

      -- Keep track of the result of running with this memory.
      mem.table.Insert(name, value);

      -- Generate new memories until we find a new one.
      loop
         Update_Memory(mem, value);
         declare
            new_name : constant Unbounded_String := To_String(mem);
            cursor   : constant Value_Maps.Cursor := mem.table.Find(new_name);
         begin
            exit when Value_Maps."="(cursor, Value_Maps.No_Element);
            value := Value_Maps.Element(cursor);
            Put_Line("Value: " & Value_Type'Image(value));
         end;
      end loop;

      -- Keep track of the number of iterations.
      mem.iteration := mem.iteration + 1;

   end Finish_Run;

   procedure Show_Access_Stats(mem : in out Super_Type) is
   begin
      Finish_Run(mem);
      Put_Line("Best Memory: " & To_String(mem.best_name));
      Put_Line("Best Value:  " & Value_Type'Image(mem.best_value));
      Put_Line("Best Cost:   " & Cost_Type'Image(mem.best_cost));
   end Show_Access_Stats;

   procedure Adjust(mem : in out Super_Type) is
   begin
      Adjust(Container_Type(mem));
      mem.generator  := new RNG.Generator;
      mem.last       := null;
      mem.current    := null;
   end Adjust;

   procedure Finalize(mem : in out Super_Type) is
   begin
      Set_Memory(mem, null);
      Finalize(Container_Type(mem));
      Destroy(mem.generator);
      Destroy(mem.last);
      Destroy(mem.current);
   end Finalize;

end Memory.Super;
