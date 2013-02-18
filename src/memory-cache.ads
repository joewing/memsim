
with Ada.Numerics.Discrete_Random;

package Memory.Cache is

   type Cache_Type is new Memory_Type with private;

   type Cache_Pointer is access all Cache_Type'Class;

   type Policy_Type is (LRU,        -- Least recently used
                        MRU,        -- Most recently used
                        FIFO,       -- First-in first-out
                        Random);    -- Random

   function Create_Cache(mem           : access Memory_Type'Class;
                         line_count    : Positive := 1;
                         line_size     : Positive := 8;
                         associativity : Positive := 1;
                         latency       : Time_Type := 1;
                         policy        : Policy_Type := LRU)
                         return Cache_Pointer;

   overriding
   procedure Read(mem      : in out Cache_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Cache_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Show_Access_Stats(mem : in out Cache_Type);

   overriding
   function To_String(mem : Cache_Type) return Unbounded_String;

   overriding
   procedure Finalize(mem : in out Cache_Type);

private

   type Cache_Data is record
      address  : Address_Type := Address_Type'Last;
      age      : Long_Integer := 0;
      dirty    : Boolean      := False;
   end record;

   type Cache_Data_Pointer is access Cache_Data;

   package Cache_Vectors is new Vectors(Natural, Cache_Data_Pointer);

   package RNG is new Ada.Numerics.Discrete_Random(Natural);

   type Cache_Type is new Memory_Type with record
      line_size      : Positive := 8;
      line_count     : Positive := 1;
      associativity  : Positive := 1;
      latency        : Time_Type := 1;
      data           : Cache_Vectors.Vector;
      policy         : Policy_Type := LRU;
      generator      : RNG.Generator;
      mem            : access Memory_Type'Class;
   end record;

end Memory.Cache;
