
with Ada.Containers.Vectors; use Ada.Containers;

package Memory is

   type Address_Type is mod 2 ** 32;

   type Time_Type is new Natural;

   type Memory_Type is abstract tagged limited private;

   type Memory_Pointer is access all Memory_Type'class;

   -- Start a transaction.
   procedure Start(mem : in out Memory_Type);

   -- Commit a transaction.
   procedure Commit(mem    : in out Memory_Type;
                    cycles : out Time_Type);

   procedure Read(mem      : in out Memory_Type;
                  address  : in Address_Type) is abstract;

   procedure Write(mem     : in out Memory_Type;
                   address : in Address_Type) is abstract;

   procedure Idle(mem      : in out Memory_Type;
                  cycles   : in Time_Type);

   function Get_Time(mem : Memory_Type) return Time_Type;

private

   procedure Advance(mem      : in out Memory_Type;
                     cycles   : in Time_Type);

   package Transaction_Vectors is new Vectors(Natural, Time_Type);

   type Memory_Type is abstract tagged limited record
      transactions   : Transaction_Vectors.Vector;
      time           : Time_Type := 0;
   end record;

end Memory;

