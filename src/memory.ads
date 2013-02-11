
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Finalization; use Ada.Finalization;

package Memory is

   type Address_Type is mod 2 ** 64;

   type Time_Type is new Long_Integer range 0 .. Long_Integer'Last;

   type Memory_Type is abstract new Limited_Controlled with private;

   type Memory_Pointer is access all Memory_Type'Class;

   -- Start a transaction.
   procedure Start(mem : in out Memory_Type);

   -- Commit a transaction.
   procedure Commit(mem    : in out Memory_Type;
                    cycles : out Time_Type);

   procedure Read(mem      : in out Memory_Type;
                  address  : in Address_Type;
                  size     : in Positive) is abstract;

   procedure Write(mem     : in out Memory_Type;
                   address : in Address_Type;
                   size    : in Positive) is abstract;

   procedure Idle(mem      : in out Memory_Type;
                  cycles   : in Time_Type);

   function Get_Time(mem : Memory_Type) return Time_Type;

   procedure Show_Stats(mem : in Memory_Type);

   procedure Show_Access_Stats(mem : in Memory_Type);

   procedure Destroy(mem : in out Memory_Pointer);

private

   package Transaction_Vectors is new Vectors(Natural, Time_Type);

   type Memory_Type is abstract new Limited_Controlled with record
      transactions   : Transaction_Vectors.Vector;
      time           : Time_Type := 0;
   end record;

   procedure Advance(mem      : in out Memory_Type'Class;
                     cycles   : in Time_Type);

end Memory;
