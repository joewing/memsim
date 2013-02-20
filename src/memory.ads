
with Ada.Numerics.Discrete_Random;
with Ada.Containers.Vectors; use Ada.Containers;
with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Memory is

   package RNG is new Ada.Numerics.Discrete_Random(Natural);

   type Address_Type is mod 2 ** 64;

   type Time_Type is new Long_Integer range 0 .. Long_Integer'Last;

   type Cost_Type is new Long_Integer range 0 .. Long_Integer'Last;

   type Memory_Type is abstract new Limited_Controlled with private;

   type Memory_Pointer is access all Memory_Type'Class;

   procedure Reset(mem : in out Memory_Type);

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

   procedure Show_Stats(mem : in out Memory_Type);

   procedure Show_Access_Stats(mem : in out Memory_Type);

   function To_String(mem : Memory_Type) return Unbounded_String is abstract;

   function Get_Cost(mem : Memory_Type) return Cost_Type is abstract;

   procedure Destroy(mem : in out Memory_Pointer);

private

   package Transaction_Vectors is new Vectors(Natural, Time_Type);

   type Memory_Type is abstract new Limited_Controlled with record
      transactions   : Transaction_Vectors.Vector;
      time           : Time_Type := 0;
   end record;

   procedure Advance(mem      : in out Memory_Type'Class;
                     cycles   : in Time_Type);

   function Log2(n : Natural) return Natural;

end Memory;
