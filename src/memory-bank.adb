
package body Memory.Bank is

   function Create_Bank return Bank_Pointer is
   begin
      return new Bank_Type;
   end Create_Bank;

   function Get_Data(mem      : Bank_Type;
                     address  : Address_Type) return Bank_Data is
      data : Bank_Data;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         if (address and data.mask) = data.key then
            return data;
         end if;
      end loop;
      raise Bank_Error;
   end Get_Data;

   procedure Start(mem : in out Bank_Type) is
      data : Bank_Data;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         Start(data.mem.all);
      end loop;
   end Start;

   procedure Commit(mem    : in out Bank_Type;
                    cycles : out Time_Type) is
      max_cycles  : Time_Type := 0;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         declare
            data     : constant Bank_Data := mem.banks.Element(i);
            bcycles  : Time_Type;
         begin
            Commit(data.mem.all, bcycles);
            if bcycles > max_cycles then
               max_cycles := bcycles;
            end if;
         end;
      end loop;
      Advance(mem, max_cycles);
      cycles := max_cycles;
   end Commit;

   procedure Read(mem      : in out Bank_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      data     : constant Bank_Data := Get_Data(mem, address);
      cycles   : Time_Type;
   begin
      Start(data.mem.all);
      Read(data.mem.all, address, size);
      Commit(data.mem.all, cycles);
      Advance(mem, cycles);
   end Read;

   procedure Write(mem     : in out Bank_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      data     : constant Bank_Data := Get_Data(mem, address);
      cycles   : Time_Type;
   begin
      Start(data.mem.all);
      Write(data.mem.all, address, size);
      Commit(data.mem.all, cycles);
      Advance(mem, cycles);
   end Write;

   procedure Idle(mem      : in out Bank_Type;
                  cycles   : in Time_Type) is
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         Idle(mem.banks.Element(i).mem.all, cycles);
      end loop;
      Advance(mem, cycles);
   end Idle;

   procedure Show_Access_Stats(mem : in Bank_Type) is
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         Show_Access_Stats(mem.banks.Element(i).mem.all);
      end loop;
   end Show_Access_Stats;

   procedure Add_Bank(mem  : in out Bank_Type;
                      bank : access Memory_Type'Class;
                      key  : in Address_Type;
                      mask : in Address_Type) is
      data : constant Bank_Data := Bank_Data'(bank, key, mask);
   begin
      mem.banks.Append(data);
   end Add_Bank;

   procedure Finalize(mem : in out Bank_Type) is
      data : Bank_Data;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         Destroy(Memory_Pointer(data.mem));
      end loop;
   end Finalize;

end Memory.Bank;

