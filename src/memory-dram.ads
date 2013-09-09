
package Memory.DRAM is

   type DRAM_Type is new Memory_Type with private;

   type DRAM_Pointer is access all DRAM_Type'Class;

   function Create_DRAM(cas_cycles     : Time_Type;   -- CAS latency
                        rcd_cycles     : Time_Type;   -- RCD latency
                        rp_cycles      : Time_Type;   -- Precharge latency
                        wb_cycles      : Time_Type;   -- Write-back latency
                        multiplier     : Time_Type;   -- Clock multiplier
                        word_size      : Positive;    -- Word size in bytes
                        page_size      : Positive;    -- Page size in bytes
                        page_count     : Positive;    -- Pages per bank
                        width          : Positive;    -- Channel width in bytes
                        burst_size     : Positive;    -- Burst size
                        open_page_mode : Boolean)     -- Open or closed page
                        return DRAM_Pointer;

   overriding
   function Clone(mem : DRAM_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem     : in out DRAM_Type;
                   context : in Natural);

   overriding
   procedure Read(mem      : in out DRAM_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out DRAM_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out DRAM_Type;
                  cycles   : in Time_Type);

   overriding
   function To_String(mem : DRAM_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : DRAM_Type) return Cost_Type;

   overriding
   function Get_Writes(mem : DRAM_Type) return Long_Integer;

   overriding
   function Get_Word_Size(mem : DRAM_Type) return Positive;

   overriding
   function Get_Ports(mem : DRAM_Type) return Port_Vector_Type;

   overriding
   procedure Generate(mem  : in DRAM_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

private

   type Bank_Type is record
      page     : Address_Type := Address_Type'Last;
      dirty    : Boolean      := False;
      pending  : Time_Type    := 0;
   end record;

   package Bank_Vectors is new Vectors(Natural, Bank_Type);

   type DRAM_Type is new Memory_Type with record
      banks          : Bank_Vectors.Vector;
      bank_size      : Positive;
      cas_cycles     : Time_Type;
      rcd_cycles     : Time_Type;
      rp_cycles      : Time_Type;
      wb_cycles      : Time_Type;
      access_cycles  : Time_Type;
      multiplier     : Time_Type;
      word_size      : Positive;
      page_size      : Positive;
      page_count     : Positive;
      width          : Positive;
      burst_size     : Positive;
      open_page_mode : Boolean;
      writes         : Long_Integer := 0;
   end record;

end Memory.DRAM;
