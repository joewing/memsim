
package body Memory.Option is

   function Create_Option return Option_Pointer is
      result : constant Option_Pointer := new Option_Type;
   begin
      return result;
   end Create_Option;

   function Clone(mem : Option_Type) return Memory_Pointer is
      result : constant Option_Pointer := new Option_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Permute(mem         : in out Option_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is
   begin
      mem.index := Random(generator) mod (mem.memories.Last_Index + 1);
   end Permute;

   procedure Add_Memory(mem      : in out Option_Type;
                        other    : access Memory_Type'Class) is
   begin
      mem.memories.Append(Memory_Pointer(other));
   end Add_Memory;

   function Done(mem : Option_Type) return Boolean is
   begin
      return Done(mem.memories.Element(mem.index).all);
   end Done;

   procedure Reset(mem     : in out Option_Type;
                   context : in Natural) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         Reset(mem.memories.Element(i).all, context);
      end loop;
   end Reset;

   procedure Set_Port(mem     : in out Option_Type;
                      port    : in Natural;
                      ready   : out Boolean) is
   begin
      Set_Port(mem.memories.Element(mem.index).all, port, ready);
   end Set_Port;

   procedure Read(mem      : in out Option_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Read(mem.memories.Element(mem.index).all, address, size);
   end Read;

   procedure Write(mem     : in out Option_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Write(mem.memories.Element(mem.index).all, address, size);
   end Write;

   procedure Idle(mem      : in out Option_Type;
                  cycles   : in Time_Type) is
   begin
      Idle(mem.memories.Element(mem.index).all, cycles);
   end Idle;

   function Get_Time(mem : Option_Type) return Time_Type is
   begin
      return Get_Time(mem.memories.Element(mem.index).all);
   end Get_Time;

   function Get_Writes(mem : Option_Type) return Long_Integer is
   begin
      return Get_Writes(mem.memories.Element(mem.index).all);
   end Get_Writes;

   function To_String(mem : Option_Type) return Unbounded_String is
   begin
      return To_String(mem.memories.Element(mem.index).all);
   end To_String;

   function Get_Cost(mem : Option_Type) return Cost_Type is
   begin
      return Get_Cost(mem.memories.Element(mem.index).all);
   end Get_Cost;

   function Get_Word_Size(mem : Option_Type) return Positive is
   begin
      return Get_Word_Size(mem.memories.Element(mem.index).all);
   end Get_Word_Size;

   procedure Generate(mem     : in Option_Type;
                      sigs    : in out Unbounded_String;
                      code    : in out Unbounded_String) is
   begin
      Generate(mem.memories.Element(mem.index).all, sigs, code);
   end Generate;

   function Get_Ports(mem : Option_Type) return Port_Vector_Type is
   begin
      return Get_Ports(mem.memories.Element(mem.index).all);
   end Get_Ports;

   procedure Adjust(mem : in out Option_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            ptr : constant Memory_Pointer := mem.memories.Element(i);
         begin
            mem.memories.Replace_Element(i, Clone(ptr.all));
         end;
      end loop;
   end Adjust;

   procedure Finalize(mem : in out Option_Type) is
   begin
      for i in mem.memories.First_Index .. mem.memories.Last_Index loop
         declare
            ptr : Memory_Pointer := mem.memories.Element(i);
         begin
            Destroy(ptr);
         end;
      end loop;
   end Finalize;

end Memory.Option;
