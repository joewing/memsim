
package Memory.Bank is

   type Bank_Type is new Memory_Type with private;

   type Bank_Pointer is access all Bank_Type'Class;

   Bank_Error : exception;

   function Create_Bank return Bank_Pointer;

   procedure Add_Bank(mem  : in out Bank_Type'Class;
                      bank : access Memory_Type'Class;
                      key  : in Address_Type;
                      mask : in Address_Type);

   overriding
   procedure Start(mem : in out Bank_Type);

   overriding
   procedure Commit(mem    : in out Bank_Type;
                    cycles : out Time_Type);

   overriding
   procedure Read(mem      : in out Bank_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Bank_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Bank_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Access_Stats(mem : in out Bank_Type);

   overriding
   function To_String(mem : Bank_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Bank_Type) return Natural;

   overriding
   procedure Finalize(mem : in out Bank_Type);

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

end Memory.Bank;
