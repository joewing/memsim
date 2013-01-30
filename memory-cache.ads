
with Ada.Containers.Vectors; use Ada.Containers;

package Memory.Cache is

   type Cached_Memory is new Memory_Base with private;

   type Cached_Pointer is access all Cached_Memory'class;

   procedure Read(mem      : access Cached_Memory;
                  address  : Address_Type);

   procedure Write(mem     : access Cached_Memory;
                   address : Address_Type);

   procedure Step(mem      : access Cached_Memory;
                  cycles   : Natural := 1);

   procedure Set_Memory(mem   : access Cached_Memory;
                        next  : Memory_Pointer);

   procedure Set_Line_Size(mem   : access Cached_Memory;
                           size  : Natural);

   procedure Set_Line_Count(mem     : access Cached_Memory;
                            count   : Natural);

   procedure Set_Associativity(mem           : access Cached_Memory;
                               associativity : Natural);

   function Get_Time(mem : access Cached_Memory) return Natural;

private

   type Cache_Data is record
      address  : Address_Type := 16#FFFFFFFF#;
      age      : Natural      := 0;
      dirty    : Boolean      := False;
   end record;
   type Cache_Data_Pointer is access Cache_Data;

   package Cache_Vectors is new Vectors(Natural, Cache_Data_Pointer);

   type Cached_Memory is new Memory_Base with record
      line_size      : Natural := 1;
      line_count     : Natural := 1;
      associativity  : Natural := 1;
      data           : Cache_Vectors.Vector;
      mem            : Memory_Pointer;
   end record;

end Memory.Cache;

