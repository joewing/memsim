
package Memory.SPM is

   type SPM_Type is new Memory_Type with private;

   type SPM_Pointer is access all SPM_Type'Class;

   function Create_SPM(mem       : access Memory_Type'Class;
                       size      : Natural;
                       latency   : Time_Type := 1) return SPM_Pointer;

   overriding
   procedure Read(mem      : in out SPM_Type;
                  address  : in Address_Type);

   overriding
   procedure Write(mem     : in out SPM_Type;
                   address : in Address_Type);

private

   type SPM_Type is new Memory_Type with record
      mem      : access Memory_Type'Class;
      size     : Natural;
      latency  : Time_Type;
   end record;

   procedure Show_Access_Stats(mem : in SPM_Type);

   procedure Finalize(mem : in out SPM_Type);

end Memory.SPM;
