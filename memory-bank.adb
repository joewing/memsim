
package body Memory.Bank is

   function Create_Bank(clock : Clock_Pointer) return Bank_Pointer is
      result : Bank_Pointer := new Bank_Type;
   begin
      result.clock := clock;
      return result;
   end Create_Bank;

   function Get_Data(mem      : Bank_Pointer;
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

   function Read(mem      : Bank_Pointer;
                 address  : Address_Type) is
      data : constant Bank_Data := Get_Data(mem, address);
   begin
      return Read(data.mem, address);
   end Read;

   function Write(mem     : Bank_Pointer;
                   address : Address_Type) is
      data : constant Bank_Data := Get_Data(mem, address);
   begin
      return Write(data.mem, address);
   end Write;

   procedure Add_Bank(mem  : Bank_Pointer;
                      bank : Memory_Pointer;
                      key  : Address_Type;
                      mask : Address_Type) is
      data : constant Bank_Data := Bank_Data'(bank, key, mask);
   begin
      mem.banks.Append(data);
   end Add_Bank;

end Memory.Bank;

