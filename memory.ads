
with Ada.Containers.Vectors; use Ada.Containers;

package Memory is

   type Address_Type is mod 2 ** 32;

   type Memory_Type is abstract tagged limited private;

   type Memory_Pointer is access all Memory_Type'class;

   -- Start a transaction.
   procedure Start(mem : in out Memory_Type);

   -- Commit a transaction.
   procedure Commit(mem    : in out Memory_Type;
                    cycles : out Natural);

   procedure Read(mem      : in out Memory_Type;
                  address  : Address_Type) is abstract;

   procedure Write(mem     : in out Memory_Type;
                   address : Address_Type) is abstract;

   function Get_Time(mem : Memory_Type) return Natural;

private

   procedure Advance(mem      : in out Memory_Type;
                     cycles   : Natural);

   package Transaction_Vectors is new Vectors(Natural, Natural);

   type Memory_Type is abstract tagged limited record
      transactions   : Transaction_Vectors.Vector;
      time           : Natural := 0;
   end record;

end Memory;

