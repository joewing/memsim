
package Memory.Cache is

   type Cache_Type is new Memory_Type with private;

   type Cache_Pointer is access all Cache_Type'class;

   function Create_Cache(mem           : access Memory_Type'class;
                         line_count    : Natural := 1;
                         line_size     : Natural := 1;
                         associativity : Natural := 1;
                         latency       : Time_Type := 1) return Cache_Pointer;

   overriding
   procedure Read(mem      : in out Cache_Type;
                  address  : Address_Type);

   overriding
   procedure Write(mem     : in out Cache_Type;
                   address : Address_Type);

private

   type Cache_Data is record
      address  : Address_Type := 16#FFFFFFFF#;
      age      : Natural      := 0;
      dirty    : Boolean      := False;
   end record;
   type Cache_Data_Pointer is access Cache_Data;

   package Cache_Vectors is new Vectors(Natural, Cache_Data_Pointer);

   type Cache_Type is new Memory_Type with record
      line_size      : Natural := 1;
      line_count     : Natural := 1;
      associativity  : Natural := 1;
      latency        : Time_Type := 1;
      data           : Cache_Vectors.Vector;
      mem            : access Memory_Type'class;
   end record;

end Memory.Cache;

