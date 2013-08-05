
with Device;
with Memory.Split;      use Memory.Split;
with Memory.Join;       use Memory.Join;
with Memory.Transform;  use Memory.Transform;

package body Memory.Register is

   -- Locate the position at which to insert a register and insert it.
   function Insert_Register(mem : Memory_Pointer) return Boolean is
      max_path : constant Natural := Device.Get_Max_Path;
   begin

      if Get_Path_Length(mem.all) <= max_path then
         return False;
      end if;

      if mem.all in Split_Type'Class then

         declare
            sp : constant Split_Pointer  := Split_Pointer(mem);
            np : constant Memory_Pointer := Get_Memory(sp.all);
            b0 : constant Memory_Pointer := Get_Bank(sp.all, 0);
            b1 : constant Memory_Pointer := Get_Bank(sp.all, 1);
         begin

            if Insert_Register(np) then
               return True;
            end if;

            if Insert_Register(b0) then
               return True;
            end if;

            if Insert_Register(b1) then
               return True;
            end if;

            -- Insert two registers: one for each bank.
            Set_Bank(sp.all, 0, Create_Register(b0));
            Set_Bank(sp.all, 1, Create_Register(b1));

            return True;

         end;

      elsif mem.all in Join_Type'Class then

         return False;

      elsif mem.all in Transform_Type'Class then

         declare
            tp : constant Transform_Pointer  := Transform_Pointer(mem);
            np : constant Memory_Pointer     := Get_Memory(tp.all);
            bp : constant Memory_Pointer     := Get_Bank(tp.all);
         begin

            if Insert_Register(np) then
               return True;
            end if;

            if bp /= null and then Insert_Register(bp) then
               return True;
            end if;

            if bp /= null then
               Set_Bank(tp.all, Create_Register(bp));
            else
               Set_Memory(tp.all, Create_Register(np));
            end if;

            return True;

         end;

      else

         declare
            cp : constant Container_Pointer := Container_Pointer(mem);
            np : constant Memory_Pointer := Get_Memory(cp.all);
         begin

            if Insert_Register(np) then
               return True;
            end if;

            Set_Memory(cp.all, Create_Register(np));

            return True;

         end;

      end if;

   end Insert_Register;

   procedure Insert_Registers(mem : access Memory_Type'Class) is
   begin

      -- Continue inserting registers until we either no longer exceed
      -- the max path length or we are unable to reduce the path length.
      loop
         exit when not Insert_Register(Memory_Pointer(mem));
      end loop;

   end Insert_Registers;

   function Remove_Registers(mem : Memory_Pointer) return Memory_Pointer is
   begin
      if mem = null then
         return null;
      elsif mem.all in Register_Type'Class then
         declare
            rp : Register_Pointer         := Register_Pointer(mem);
            np : constant Memory_Pointer  := Get_Memory(rp.all);
         begin
            Set_Memory(rp.all, null);
            Destroy(Memory_Pointer(rp));
            return Remove_Registers(np);
         end;
      elsif mem.all in Split_Type'Class then
         declare
            sp : constant Split_Pointer   := Split_Pointer(mem);
            b0 : constant Memory_Pointer  := Get_Bank(sp.all, 0);
            b1 : constant Memory_Pointer  := Get_Bank(sp.all, 1);
            np : constant Memory_Pointer  := Get_Memory(sp.all);
         begin
            Set_Bank(sp.all, 0, Remove_Registers(b0));
            Set_Bank(sp.all, 1, Remove_Registers(b1));
            Set_Memory(sp.all, Remove_Registers(np));
            return mem;
         end;
      elsif mem.all in Transform_Type'Class then
         declare
            tp : constant Transform_Pointer  := Transform_Pointer(mem);
            bp : constant Memory_Pointer     := Get_Bank(tp.all);
            np : constant Memory_Pointer     := Get_Memory(tp.all);
         begin
            Set_Bank(tp.all, Remove_Registers(bp));
            Set_Memory(tp.all, Remove_Registers(np));
            return mem;
         end;
      elsif mem.all in Container_Type'Class then
         declare
            cp : constant Container_Pointer  := Container_Pointer(mem);
            np : constant Memory_Pointer     := Get_Memory(cp.all);
         begin
            Set_Memory(cp.all, Remove_Registers(np));
            return mem;
         end;
      else
         return mem;
      end if;
   end Remove_Registers;

   function Create_Register(mem : access Memory_Type'Class)
                            return Register_Pointer is
      result : constant Register_Pointer := new Register_Type;
   begin
      Set_Memory(result.all, mem);
      return result;
   end Create_Register;

   function Clone(mem : Register_Type) return Memory_Pointer is
   begin
      return new Register_Type'(mem);
   end Clone;

   procedure Permute(mem         : in out Register_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is
   begin
      null;
   end Permute;

   procedure Read(mem      : in out Register_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Advance(mem, 1);
      Read(Container_Type(mem), address, size);
   end Read;

   procedure Write(mem     : in out Register_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Advance(mem, 1);
      Write(Container_Type(mem), address, size);
   end Write;

   function Get_Path_Length(mem : Register_Type) return Natural is
   begin
      return 0;
   end Get_Path_Length;

   function To_String(mem : Register_Type) return Unbounded_String is
      result : Unbounded_String;
   begin
      Append(result, "(register ");
      Append(result, "(memory ");
      Append(result, To_String(Container_Type(mem)));
      Append(result, ")");
      Append(result, ")");
      return result;
   end To_String;

   procedure Generate(mem  : in Register_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is
      other : constant Memory_Pointer  := Get_Memory(mem);
      wbits : constant Natural         := 8 * Get_Word_Size(mem);
      name  : constant String          := "m" & To_String(Get_ID(mem));
      oname : constant String          := "m" & To_String(Get_ID(other.all));
   begin

      Generate(other.all, sigs, code);
      Declare_Signals(sigs, name, wbits);

      Line(code, name & "_inst : entity work.reg");
      Line(code, "   generic map (");
      Line(code, "      ADDR_WIDTH => ADDR_WIDTH,");
      Line(code, "      WORD_WIDTH => " & To_String(wbits));
      Line(code, "   )");
      Line(code, "   port map (");
      Line(code, "      clk      => clk,");
      Line(code, "      rst      => rst,");
      Line(code, "      addr     => " & name & "_addr,");
      Line(code, "      din      => " & name & "_din,");
      Line(code, "      dout     => " & name & "_dout,");
      Line(code, "      re       => " & name & "_re,");
      Line(code, "      we       => " & name & "_we,");
      Line(code, "      mask     => " & name & "_mask,");
      Line(code, "      ready    => " & name & "_ready,");
      Line(code, "      maddr    => " & oname & "_addr,");
      Line(code, "      min      => " & oname & "_dout,");
      Line(code, "      mout     => " & oname & "_din,");
      Line(code, "      mre      => " & oname & "_re,");
      Line(code, "      mwe      => " & oname & "_we,");
      Line(code, "      mmask    => " & oname & "_mask,");
      Line(code, "      mready   => " & oname & "_ready");
      Line(code, "   );");

   end Generate;

end Memory.Register;
