
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Assertions;          use Ada.Assertions;
with Memory.Cache;            use Memory.Cache;
with Memory.Transform.Offset; use Memory.Transform.Offset;
with Memory.Transform.Shift;  use Memory.Transform.Shift;
with Benchmark;

package body Memory.Super is

   function Create_Memory(mem    : Super_Type;
                          cost   : Cost_Type)
                          return Container_Pointer is
      result : Memory_Pointer;
   begin
      case RNG.Random(mem.generator.all) mod 4 is
         when 0      =>    -- Shift
            result := Random_Shift(mem.generator.all, cost);
         when 1      =>    -- Offset
            result := Random_Offset(mem.generator.all, cost);
         when others =>    -- Cache
            result := Random_Cache(mem.generator.all, cost);
      end case;
      return Container_Pointer(result);
   end Create_Memory;

   function Clone(mem : Super_Type) return Memory_Pointer is
      result : constant Super_Pointer := new Super_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Randomize(mem : in out Super_Type) is
      next  : Memory_Pointer := null;
      temp  : Container_Pointer := null;
      len   : constant Natural := Natural(mem.chain.Length);
      pos   : Natural;
      cost  : constant Cost_Type := Get_Cost(mem);
      left  : constant Cost_Type := mem.max_cost - cost;
   begin

      -- Unlink memories.
      for i in mem.chain.First_Index .. mem.chain.Last_Index loop
         Set_Memory(mem.chain.Element(i).all, null);
      end loop;
      Set_Memory(mem, null);

      -- Select an action to take.
      -- Here we give extra weight to modifying an existing hierarchy
      -- rather than adding or removing stages.
      case RNG.Random(mem.generator.all) mod 16 is
         when 0      =>    -- Insert a stage.
            temp := Create_Memory(mem, left);
            if temp /= null then
               pos := RNG.Random(mem.generator.all) mod (len + 1);
               mem.chain.Insert(Memory_Vectors.Extended_Index(pos), temp);
            elsif len > 0 then
               pos := RNG.Random(mem.generator.all) mod len;
               temp := mem.chain.Element(pos);
               Destroy(Memory_Pointer(temp));
               mem.chain.Delete(pos);
            end if;
         when 1      =>    -- Remove a stage (unless there are none).
            if len = 0 then
               temp := Create_Memory(mem, left);
               if temp /= null then
                  pos := RNG.Random(mem.generator.all) mod (len + 1);
                  mem.chain.Insert(pos, temp);
               end if;
            else
               pos := RNG.Random(mem.generator.all) mod len;
               temp := mem.chain.Element(pos);
               Destroy(Memory_Pointer(temp));
               mem.chain.Delete(pos);
            end if;
         when others =>    -- Modify an existing stage.
            if len = 0 then
               temp := Create_Memory(mem, left);
               if temp /= null then
                  mem.chain.Append(temp);
               end if;
            else
               pos := RNG.Random(mem.generator.all) mod len;
               temp := mem.chain.Element(pos);
               Permute(temp.all, mem.generator.all, left + Get_Cost(temp.all));
            end if;
      end case;

      -- Link the memories together.
      next := mem.dram;
      for i in reverse mem.chain.First_Index .. mem.chain.Last_Index loop
         declare
            temp : constant Container_Pointer := mem.chain.Element(i);
         begin
            Set_Memory(temp.all, next);
            next := Memory_Pointer(temp);
         end;
      end loop;
      Set_Memory(mem, next);

      Put_Line(To_String(To_String(mem)));
      Assert(Get_Cost(mem) <= mem.max_cost, "Invalid randomize");

   end Randomize;

   function Create_Super(max_cost   : Cost_Type;
                         dram       : not null access Memory_Type'Class)
                         return Super_Pointer is
      result : constant Super_Pointer := new Super_Type;
   begin
      result.max_cost := max_cost;
      result.dram := Memory_Pointer(dram);
      return result;
   end Create_Super;

   procedure Finish_Run(mem : in out Super_Type) is
      time : constant Time_Type := Get_Time(mem);
      cost : constant Cost_Type := Get_Cost(mem);
      temp : Container_Pointer;
      next : Memory_Pointer;
   begin

      -- Keep track of the best memory.
      if time < mem.best_time or else
         (time = mem.best_time and cost < mem.best_cost) then
         mem.best_time := time;
         mem.best_cost := cost;
         mem.best_name := To_String(mem);
      end if;

      -- Determine if we should keep this memory for the next
      -- run or revert the the previous memory.
      if time < mem.last_time or else
            (time = mem.last_time and cost < mem.last_cost) or else
            (RNG.Random(mem.generator.all) mod 8) = 0 then

         -- This memory is better than the last or we decide to
         -- select a worse memory this time.
         -- The last chain will be destroyed later.
         mem.last_time := time;
         mem.last_cost := Get_Cost(mem);

      else

         -- This memory is not as good as the last and we do not
         -- want a worse memory this time.
         -- In this case, we need to destroy chain and copy last_chain
         -- to chain.
         for i in mem.chain.First_Index .. mem.chain.Last_Index loop
            temp := mem.chain.Element(i);
            Set_Memory(temp.all, null);
            Destroy(Memory_Pointer(temp));
         end loop;
         mem.chain := mem.last_chain;
         mem.last_chain.Clear;

         -- Fix links.
         next := mem.dram;
         for i in reverse mem.chain.First_Index .. mem.chain.Last_Index loop
            temp := mem.chain.Element(i);
            Set_Memory(temp.all, next);
            next := Memory_Pointer(temp);
         end loop;
         Set_Memory(mem, next);

      end if;
   end Finish_Run;

   procedure Reset(mem : in out Super_Type) is
      temp : Container_Pointer;
      next : Memory_Pointer;
   begin

      -- Reset the time.
      Reset(Container_Type(mem));

      -- Destroy the last chain.
      for i in mem.last_chain.First_Index .. mem.last_chain.Last_Index loop
         temp := mem.last_chain.Element(i);
         Set_Memory(temp.all, null);
         Destroy(Memory_Pointer(temp));
      end loop;
      mem.last_chain.Clear;

      -- Keep a copy of the current memory so we can go back to it
      -- if our random change is no good.
      for i in mem.chain.First_Index .. mem.chain.Last_Index loop
         temp := mem.chain.Element(i);
         Set_Memory(temp.all, null);
         temp := Container_Pointer(Clone(temp.all));
         mem.last_chain.Append(temp);
      end loop;

      -- Fix links.
      next := mem.dram;
      for i in reverse mem.chain.First_Index .. mem.chain.Last_Index loop
         temp := mem.chain.Element(i);
         Set_Memory(temp.all, next);
         next := Memory_Pointer(temp);
      end loop;
      Set_Memory(mem, next);

      -- Make a random modification to the memory.
      Randomize(mem);

   end Reset;

   procedure Read(mem      : in out Super_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Read(Container_Type(mem), address, size);
      if Get_Time(mem) > mem.best_time then
         raise Benchmark.Timeout;
      end if;
   end Read;

   procedure Write(mem     : in out Super_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Write(Container_Type(mem), address, size);
      if Get_Time(mem) >= mem.best_time then
         raise Benchmark.Timeout;
      end if;
   end Write;

   procedure Idle(mem      : in out Super_Type;
                  cycles   : in Time_Type) is
   begin
      Idle(Container_Type(mem), cycles);
      if Get_Time(mem) >= mem.best_time then
         raise Benchmark.Timeout;
      end if;
   end Idle;

   procedure Show_Access_Stats(mem : in out Super_Type) is
   begin
      Finish_Run(mem);
      Put_Line("Best Memory: " & To_String(mem.best_name));
      Put_Line("Best Time:   " & Time_Type'Image(mem.best_time));
      Put_Line("Best Cost:   " & Cost_Type'Image(mem.best_cost));
   end Show_Access_Stats;

   procedure Adjust(mem : in out Super_Type) is
   begin
      mem.dram       := null;
      mem.generator  := new RNG.Generator;
      mem.chain.Clear;
      mem.last_chain.Clear;
   end Adjust;

   procedure Finalize(mem : in out Super_Type) is
   begin
      Destroy(mem.generator);
   end Finalize;

end Memory.Super;
