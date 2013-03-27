
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
                          cost      : Cost_Type)
                          return Container_Pointer;

   function Create_Split(mem        : Super_Type;
                         cost       : Cost_Type)
                         return Container_Pointer;

   function Create_Memory(mem       : Super_Type;
                          cost      : Cost_Type)
                          return Container_Pointer is
      result : Memory_Pointer;
   begin
      case RNG.Random(mem.generator.all) mod 8 is
         when 0      =>    -- Shift (1/8)
            result := Random_Shift(mem.generator.all, cost);
         when 1      =>    -- Offset (1/8)
            result := Random_Offset(mem.generator.all, cost);
         when 2      =>    -- Strided prefetch (1/8)
            result := Random_Prefetch(mem.generator.all, cost);
         when 3      =>    -- Split (1/8)
            result := Memory_Pointer(Create_Split(mem, cost));
         when 4      =>    -- SPM (1/8)
            result := Random_SPM(mem.generator.all, cost);
         when others =>    -- Cache (3/8)
            result := Random_Cache(mem.generator.all, cost);
      end case;
      return Container_Pointer(result);
   end Create_Memory;

   function Create_Split(mem        : Super_Type;
                         cost       : Cost_Type)
                         return Container_Pointer is
      result   : Split_Pointer
                  := Split_Pointer(Random_Split(mem.generator.all, cost));
      bank0    : Container_Pointer := Create_Memory(mem, cost / 2);
      bank1    : Container_Pointer := Create_Memory(mem, cost / 2);
      join0    : Join_Pointer := Create_Join;
      join1    : Join_Pointer := Create_Join;
   begin
      if bank0 /= null and bank1 /= null then
         Set_Memory(bank0.all, join0);
         Set_Memory(bank1.all, join1);
         Set_Bank(result, 0, bank0);
         Set_Bank(result, 1, bank1);
         return Container_Pointer(result);
      else
         if bank0 /= null then
            Destroy(Memory_Pointer(bank0));
         end if;
         if bank1 /= null then
            Destroy(Memory_Pointer(bank1));
         end if;
         Destroy(Memory_Pointer(join0));
         Destroy(Memory_Pointer(join1));
         Destroy(Memory_Pointer(result));
         return null;
      end if;
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

            if 1 + ac >= index then

               -- The memory to remove is in the first bank.
               Remove_Memory(a, index - 1);
               Set_Bank(sp, 0, a);

            elsif 1 + ac + bc >= index then

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

   procedure Insert_Memory(ptr   : in out Memory_Pointer;
                           index : in Natural;
                           other : in Container_Pointer) is
   begin

      if index = 0 then

         -- Insert other before ptr.
         Set_Memory(other.all, ptr);
         ptr := Memory_Pointer(other);

      elsif ptr.all in Split_Type'Class then

         declare
            sp : constant Split_Pointer   := Split_Pointer(ptr);
            a  : Memory_Pointer           := Get_Bank(sp, 0);
            ac : constant Natural         := Count_Memories(a);
            b  : Memory_Pointer           := Get_Bank(sp, 1);
            bc : constant Natural         := Count_Memories(b);
            n  : Memory_Pointer           := Get_Memory(sp.all);
         begin

            if 1 + ac >= index then

               -- Insert to the first bank.
               Insert_Memory(a, index - 1, other);
               Set_Bank(sp, 0, a);

            elsif 1 + ac + bc >= index then

               -- Insert to the second bank.
               Insert_Memory(b, index - ac - 1, other);
               Set_Bank(sp, 1, b);

            else

               -- Insert after the split.
               Insert_Memory(n, index - ac - bc - 1, other);
               Set_Memory(sp.all, n);

            end if;

         end;

      elsif ptr.all in Container_Type'Class then
         declare
            cp : constant Container_Pointer  := Container_Pointer(ptr);
            n  : Memory_Pointer              := Get_Memory(cp.all);
         begin
            Insert_Memory(n, index - 1, other);
            Set_Memory(cp.all, n);
         end;
      end if;

   end Insert_Memory;

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
            temp := Create_Memory(mem, left);
            if temp /= null then
               pos := RNG.Random(mem.generator.all) mod (len + 1);
               Insert_Memory(mem.current, pos, temp);
            elsif len > 0 then
               pos := RNG.Random(mem.generator.all) mod len;
               Remove_Memory(mem.current, pos);
            end if;
         when 1 .. 2 =>    -- Remove a component.
            if len = 0 then
               temp := Create_Memory(mem, left);
               if temp /= null then
                  pos := RNG.Random(mem.generator.all) mod (len + 1);
                  Insert_Memory(mem.current, pos, temp);
               end if;
            else
               pos := RNG.Random(mem.generator.all) mod len;
               Remove_Memory(mem.current, pos);
            end if;
         when others =>    -- Modify a component.
            if len = 0 then
               temp := Create_Memory(mem, left);
               if temp /= null then
                  Insert_Memory(mem.current, 0, temp);
               end if;
            else
               pos := RNG.Random(mem.generator.all) mod len;
               temp := Get_Memory(mem.current, pos);
               Permute(temp.all, mem.generator.all, left + Get_Cost(temp.all));
            end if;
      end case;
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
      mem.temperature := mem.temperature * 0.9;
      if mem.temperature < 1.0 / Float(Natural'Last) then
         mem.temperature := enew;
      end if;

      -- Determine if we should keep this memory for the next
      -- run or revert the the previous memory.
      if value <= mem.last_value or rand < prob or mem.iteration = 0 then

         -- Keep the current memory.
         mem.last_value := value;
         mem.last_cost := Get_Cost(mem);

         -- Destroy the last memory.
         Destroy(mem.last);

         -- Keep a copy of the current memory.
         mem.last := Clone(mem.current.all);

      else

         -- Revert to the previous memory.

         -- Destroy this memory.
         Destroy(mem.current);

         -- Copy the last memory to the current memory.
         mem.current := Clone(mem.last.all);
         Set_Memory(mem, mem.current);

      end if;

      -- Make a random modification to the memory.
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
            Length(To_String(mem)) < Length(mem.best_name)) then
         mem.best_value := value;
         mem.best_cost  := cost;
         mem.best_name  := To_String(mem);
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
      Finalize(Container_Type(mem));
      Destroy(mem.generator);
      Destroy(mem.last);
      Destroy(mem.current);
   end Finalize;

end Memory.Super;
