
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO;    use Ada.Text_IO;

package body Memory.Dup is

   function Create_Dup return Dup_Pointer is
   begin
      return new Dup_Type;
   end Create_Dup;

   function Clone(mem : Dup_Type) return Memory_Pointer is
      result : constant Dup_Pointer := new Dup_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Add_Memory(mem   : in out Dup_Type;
                        other : access Memory_Type'Class) is
   begin
      mem.memories.Append(Memory_Pointer(other));
   end Add_Memory;

   procedure Update_Time(mem : in out Dup_Type) is
      max_time : Time_Type := 0;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            t : constant Time_Type := mem.memories.Element(i).time;
         begin
            max_time := max_time + t;
         end;
      end loop;
      mem.time := max_time;
   end Update_Time;

   procedure Reset(mem     : in out Dup_Type;
                   context : in Natural) is
   begin
      Reset(Memory_Type(mem), context);
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Reset(mem.memories.Element(i).all, context);
      end loop;
   end Reset;

   procedure Read(mem      : in out Dup_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Read(mem.memories.Element(i).all, address, size);
      end loop;
      Update_Time(mem);
   end Read;

   procedure Write(mem     : in out Dup_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Write(mem.memories.Element(i).all, address, size);
      end loop;
      Update_Time(mem);
   end Write;

   procedure Idle(mem      : in out Dup_Type;
                  cycles   : in Time_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Idle(mem.memories.Element(i).all, cycles);
      end loop;
      Update_Time(mem);
   end Idle;

   procedure Show_Stats(mem : in out Dup_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            other : constant Memory_Pointer := mem.memories.Element(i);
         begin
            Put(Integer'Image(i) & ": ");
            Show_Stats(other.all);
         end;
      end loop;
   end Show_Stats;

   function To_String(mem : Dup_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(dup ");
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            other : constant Memory_Pointer := mem.memories.Element(i);
         begin
            Append(result, To_String(other.all));
         end;
      end loop;
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Dup_Type) return Cost_Type is
      result   : Cost_Type := 0;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         result := result + Get_Cost(mem.memories.Element(i).all);
      end loop;
      return result;
   end Get_Cost;

   function Get_Writes(mem : Dup_Type) return Long_Integer is
      result : Long_Integer := 0;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         result := result + Get_Writes(mem.memories.Element(i).all);
      end loop;
      return result;
   end Get_Writes;

   function Get_Word_Size(mem : Dup_Type) return Positive is
      temp   : Positive;
      result : Positive := 1;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         temp := Get_Word_Size(mem.memories.Element(i).all);
         result := Positive'Max(result, temp);
      end loop;
      return result;
   end Get_Word_Size;

   function Get_Ports(mem : Dup_Type) return Port_Vector_Type is
      result : Port_Vector_Type;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            ptr  : constant Memory_Pointer := mem.memories.Element(i);
            pvec : constant Port_Vector_Type := Get_Ports(ptr.all);
         begin
            result.Append(pvec);
         end;
      end loop;
      return result;
   end Get_Ports;

   procedure Generate(mem  : in Dup_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
   begin
      Assert(False, "Memory.Dup.Generate not implemented");
   end Generate;

   procedure Adjust(mem : in out Dup_Type) is
      ptr : Memory_Pointer;
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         ptr := Clone(mem.memories.Element(i).all);
         mem.memories.Replace_Element(i, ptr);
      end loop;
   end;

   procedure Finalize(mem : in out Dup_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            ptr : Memory_Pointer := mem.memories.Element(i);
         begin
            Destroy(ptr);
         end;
      end loop;
   end Finalize;

end Memory.Dup;
