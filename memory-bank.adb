
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
                    cycles : out Natural) is
      max_cycles  : Natural := 0;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         declare
            data     : constant Bank_Data := mem.banks.Element(i);
            bcycles  : Natural;
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
                  address  : Address_Type) is
      data     : constant Bank_Data := Get_Data(mem, address);
      cycles   : Natural;
   begin
      Start(data.mem.all);
      Read(data.mem.all, address);
      Commit(data.mem.all, cycles);
      Advance(mem, cycles);
   end Read;

   procedure Write(mem     : in out Bank_Type;
                   address : Address_Type) is
      data     : constant Bank_Data := Get_Data(mem, address);
      cycles   : Natural;
   begin
      Start(data.mem.all);
      Write(data.mem.all, address);
      Commit(data.mem.all, cycles);
      Advance(mem, cycles);
   end Write;

   procedure Add_Bank(mem  : in out Bank_Type;
                      bank : access Memory_Type'class;
                      key  : Address_Type;
                      mask : Address_Type) is
      data : constant Bank_Data := Bank_Data'(bank, key, mask);
   begin
      Bank_Vectors.Append(mem.banks, data);
   end Add_Bank;

end Memory.Bank;

