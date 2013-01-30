
package body Memory.Bank is

   function Get_Data(mem      : access Banked_Memory;
                     address  : Address_Type) return Bank_Data is
      data : Bank_Data;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         if (address and data.mask) = data.key then
            return data;
         end if;
      end loop;
      return Bank_Data'(null, 0, 0);
   end Get_Data;

   procedure Read(mem      : access Banked_Memory;
                  address  : Address_Type) is
      data : constant Bank_Data := Get_Data(mem, address);
   begin
      Read(data.mem, address);
   end Read;

   procedure Write(mem     : access Banked_Memory;
                   address : Address_Type) is
      data : constant Bank_Data := Get_Data(mem, address);
   begin
      Write(data.mem, address);
   end Write;

   procedure Step(mem      : access Banked_Memory;
                  cycles   : Natural := 1) is
      data     : Bank_Data;
      time     : Natural;
      max_time : Natural := 0;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         time := Get_Time(data.mem);
         if time > max_time then
            max_time := time;
         end if;
      end loop;
      max_time := max_time + cycles;
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         Set_Time(data.mem, max_time);
      end loop;
   end Step;

   procedure Add_Bank(mem  : access Banked_Memory;
                      bank : Memory_Pointer;
                      key  : Address_Type;
                      mask : Address_Type) is
      data : constant Bank_Data := Bank_Data'(bank, key, mask);
   begin
      mem.banks.Append(data);
   end Add_Bank;

end Memory.Bank;

