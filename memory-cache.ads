
with Ada.Containers.Vectors; use Ada.Containers;

package Memory.Cache is

   type Cache_Type is new Memory_Type with private;

   type Cache_Pointer is access Cache_Type;

   function Create_Cache(mem           : Memory_Pointer;
                         line_count    : Natural := 1;
                         line_size     : Natural := 1;
                         associativity : Natural := 1) return Cache_Pointer;

   function Read(mem      : Cache_Pointer;
                 address  : Address_Type) return Natural;

   function Write(mem     : Cache_Pointer;
                  address : Address_Type) return Natural;

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

