
package Memory.Bank is

   type Bank_Type is new Memory_Type with private;

   type Bank_Pointer is access all Bank_Type'Class;

   Bank_Error : exception;

   function Create_Bank return Bank_Pointer;

   overriding
   procedure Start(mem : in out Bank_Type);

   overriding
   procedure Commit(mem    : in out Bank_Type;
                    cycles : out Time_Type);

   overriding
   procedure Read(mem      : in out Bank_Type;
                  address  : in Address_Type);

   overriding
   procedure Write(mem     : in out Bank_Type;
                   address : in Address_Type);

   overriding
   procedure Idle(mem      : in out Bank_Type;
                  cycles   : in Time_Type);

   procedure Add_Bank(mem  : in out Bank_Type;
                      bank : access Memory_Type'Class;
                      key  : in Address_Type;
                      mask : in Address_Type);

private

   type Bank_Data is record
      mem      : access Memory_Type'Class;
      key      : Address_Type;
      mask     : Address_Type;
   end record;

   package Bank_Vectors is new Vectors(Natural, Bank_Data);

   type Bank_Type is new Memory_Type with record
      banks    : Bank_Vectors.Vector;
   end record;

   overriding
   procedure Show_Access_Stats(mem : in Bank_Type);

   overriding
   procedure Finalize(mem : in out Bank_Type);

end Memory.Bank;

