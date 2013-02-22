
package body Memory.SPM is

   function Create_SPM(mem       : access Memory_Type'Class;
                       size      : Natural;
                       latency   : Time_Type := 1) return SPM_Pointer is
      result : constant SPM_Pointer := new SPM_Type;
   begin
      Set_Memory(result.all, mem);
      result.size := size;
      result.latency := latency;
      return result;
   end Create_SPM;

   function Clone(mem : SPM_Type) return Memory_Pointer is
      result : constant SPM_Pointer := new SPM_Type'(mem);
   begin
      return Memory_Pointer(result);
   end Clone;

   procedure Read(mem      : in out SPM_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Forward_Start(mem);
         Forward_Read(mem, address, size);
         Forward_Commit(mem, cycles);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            diff  : constant Address_Type := naddr - address;
            nsize : constant Positive := mem.size - Positive(diff);
         begin
            Forward_Start(mem);
            Forward_Read(mem, naddr, nsize);
            Forward_Commit(mem, cycles);
         end;
      end if;
      Advance(mem, cycles);
   end Read;

   procedure Write(mem     : in out SPM_Type;
                   address : in Address_Type;
                   size    : in Positive) is
      cycles : Time_Type := mem.latency;
   begin
      if address >= Address_Type(mem.size) then
         Forward_Start(mem);
         Forward_Write(mem, address, size);
         Forward_Commit(mem, cycles);
      elsif address + Address_Type(size) > Address_Type(mem.size) then
         declare
            naddr : constant Address_Type := Address_Type(mem.size);
            diff  : constant Address_Type := naddr - address;
            nsize : constant Positive := mem.size - Positive(diff);
         begin
            Forward_Start(mem);
            Forward_Write(mem, naddr, nsize);
            Forward_Commit(mem, cycles);
         end;
      end if;
      Advance(mem, cycles);
   end Write;

   function To_String(mem : SPM_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(spm ");
      Append(result, "(size" & Natural'Image(mem.size) & ")");
      Append(result, "(latency" & Time_Type'Image(mem.latency) & ")");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   function Get_Cost(mem : SPM_Type) return Cost_Type is
   begin
      return 6 * 8 * Cost_Type(mem.size);
   end Get_Cost;

end Memory.SPM;
