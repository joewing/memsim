
package Memory.Cache is

   type Cache_Type is new Memory_Type with private;

   type Cache_Pointer is access all Cache_Type'Class;

   function Create_Cache(mem           : access Memory_Type'Class;
                         line_count    : Positive := 1;
                         line_size     : Positive := 8;
                         associativity : Positive := 1;
                         latency       : Time_Type := 1) return Cache_Pointer;

   overriding
   procedure Read(mem      : in out Cache_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Cache_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Show_Access_Stats(mem : in Cache_Type);

   overriding
   procedure Finalize(mem : in out Cache_Type);

private

   type Cache_Data is record
      address  : Address_Type := Address_Type'Last;
      age      : Natural      := 0;
      dirty    : Boolean      := False;
   end record;

   type Cache_Data_Pointer is access Cache_Data;

   package Cache_Vectors is new Vectors(Natural, Cache_Data_Pointer);

   type Cache_Type is new Memory_Type with record
      line_size      : Positive := 8;
      line_count     : Positive := 1;
      associativity  : Positive := 1;
      latency        : Time_Type := 1;
      data           : Cache_Vectors.Vector;
      mem            : access Memory_Type'Class;
   end record;

end Memory.Cache;
