
package body Memory.Bank is

   function Create_Bank return Bank_Pointer is
   begin
      return new Bank_Type;
   end Create_Bank;

   function Clone(mem : Bank_Type) return Memory_Pointer is
      result : constant Bank_Pointer := new Bank_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Add_Bank(mem  : in out Bank_Type'Class;
                      bank : access Memory_Type'Class;
                      key  : in Address_Type;
                      mask : in Address_Type) is
      data : constant Bank_Data := Bank_Data'(bank, key, mask, 0);
   begin
      mem.banks.Append(data);
   end Add_Bank;

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

   procedure Reset(mem : in out Bank_Type) is

      procedure Reset_Bank(data : in out Bank_Data) is
      begin
         Reset(data.mem.all);
         data.pending := 0;
      end Reset_Bank;

   begin
      Reset(Memory_Type(mem));
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         mem.banks.Update_Element(i, Reset_Bank'Access);
      end loop;
   end Reset;

   procedure Read(mem      : in out Bank_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      data        : constant Bank_Data := Get_Data(mem, address);
      start_time  : constant Time_Type := Get_Time(data.mem.all);
   begin
      Read(data.mem.all, address, size);
      Advance(mem, Get_Time(data.mem.all) - start_time);
   end Read;

   procedure Write(mem     : in out Bank_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      data        : constant Bank_Data := Get_Data(mem, address);
      start_time  : constant Time_Type := Get_Time(data.mem.all);
   begin
      Write(data.mem.all, address, size);
      Advance(mem, Get_Time(data.mem.all) - start_time);
   end Write;

   procedure Idle(mem      : in out Bank_Type;
                  cycles   : in Time_Type) is
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         Idle(mem.banks.Element(i).mem.all, cycles);
      end loop;
      Advance(mem, cycles);
   end Idle;

   procedure Show_Access_Stats(mem : in out Bank_Type) is
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         Show_Access_Stats(mem.banks.Element(i).mem.all);
      end loop;
   end Show_Access_Stats;

   function To_String(mem : Bank_Type) return Unbounded_String is
      data     : Bank_Data;
      result   : Unbounded_String;
   begin
      Append(result, "(bank ");
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         Append(result, "(");
         Append(result, "(key" & Address_Type'Image(data.key) & ") ");
         Append(result, "(mask" & Address_Type'Image(data.mask) & ") ");
         Append(result, "(memory ");
         Append(result, To_String(data.mem.all));
         Append(result, ")");
      end loop;
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : Bank_Type) return Cost_Type is
      data     : Bank_Data;
      result   : Cost_Type := 0;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         result := result + Get_Cost(data.mem.all);
      end loop;
      return result;
   end Get_Cost;

   function Get_Writes(mem : Bank_Type) return Long_Integer is
      result : Long_Integer := 0;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         result := result + Get_Writes(mem.banks.Element(i).mem.all);
      end loop;
      return result;
   end Get_Writes;

   procedure Adjust(mem : in out Bank_Type) is
      data : Bank_Data;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         data.mem := Clone(data.mem.all);
         mem.banks.Replace_Element(i, data);
      end loop;
   end Adjust;

   procedure Finalize(mem : in out Bank_Type) is
      data : Bank_Data;
   begin
      for i in mem.banks.First_Index .. mem.banks.Last_Index loop
         data := mem.banks.Element(i);
         Destroy(Memory_Pointer(data.mem));
      end loop;
   end Finalize;

end Memory.Bank;

