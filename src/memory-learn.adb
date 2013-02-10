
with Ada.Text_IO; use Ada.Text_IO;

package body Memory.Learn is

   function Create_Learn(mem : access Memory_Type'Class)
                         return Learn_Pointer is
      result : constant Learn_Pointer := new Learn_Type;
   begin
      result.mem := mem;
      for i in result.parameters'Range loop
         result.parameters(i) := 0.5;
      end loop;
      return result;
   end Create_Learn;

   procedure Start(mem : in out Learn_Type) is
   begin
      if mem.mem /= null then
         Start(mem.mem.all);
         mem.time := mem.mem.time;
      end if;
   end Start;

   procedure Commit(mem    : in out Learn_Type;
                    cycles : out Time_Type) is
   begin
      if mem.mem /= null then
         Commit(mem.mem.all, cycles);
         mem.time := mem.mem.time;
      end if;
   end Commit;

   function Get_Expected(mem     : in Learn_Type;
                         address : in Address_Type) return Address_Type is
      index    : Integer := mem.parameters'First;
      result   : Address_Type := 0;
      score    : Float;
   begin
      for dest in 0 .. Address_Type'Size - 1 loop
         score := 0.0;
         for src in 0 .. Address_Type'Size - 1 loop
            if (address and (2 ** src)) /= 0 then
               score := score + mem.parameters(index);
            else
               score := score + 1.0 - mem.parameters(index);
            end if;
            index := index + 1;
         end loop;
         if score >= 0.5 * Float(Address_Type'Size) then
            result := result or (2 ** dest);
         end if;
      end loop;
      return result;
   end Get_Expected;

   procedure Update_Parameters(mem     : in out Learn_Type;
                               address : in Address_Type) is

      index    : Natural := mem.parameters'First;
      actual   : Boolean;

   begin

      for dest in 0 .. Address_Type'Size - 1 loop
         actual := (address and (2 ** dest)) /= 0;
         for src in 0 .. Address_Type'Size - 1 loop
            if actual and mem.parameters(index) < 1.0 then
               mem.parameters(index) := mem.parameters(index) + mem.epsilon;
            elsif not actual and mem.parameters(index) > 0.0 then
               mem.parameters(index) := mem.parameters(index) - mem.epsilon;
            end if;
            index := index + 1;
         end loop;
      end loop;

      mem.expected := Get_Expected(mem, address);

   end Update_Parameters;

   procedure Read(mem      : in out Learn_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      if mem.mem /= null then
         Read(mem.mem.all, address, size);
         mem.time := mem.mem.time;
      end if;
      Update_Parameters(mem, address);
   end Read;

   procedure Write(mem     : in out Learn_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      if mem.mem /= null then
         Write(mem.mem.all, address, size);
         mem.time := mem.mem.time;
      end if;
      Update_Parameters(mem, address);
   end Write;

   procedure Idle(mem      : in out Learn_Type;
                  cycles   : in Time_Type) is
   begin
      if mem.mem /= null then
         Idle(mem.mem.all, cycles);
         mem.time := mem.mem.time;
      end if;
   end Idle;

   procedure Show_Access_Stats(mem : in Learn_Type) is
   begin
      if mem.mem /= null then
         Show_Access_Stats(mem.mem.all);
      end if;
      Put_Line("Parameters:");
      for i in mem.parameters'Range loop
         Put_Line(Natural'Image(i) & ":" & Float'Image(mem.parameters(i)));
      end loop;
   end Show_Access_Stats;

end Memory.Learn;
