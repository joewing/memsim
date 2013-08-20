
with Memory.Container; use Memory.Container;

package Memory.Cache is

   type Cache_Type is new Container_Type with private;

   type Cache_Pointer is access all Cache_Type'Class;

   type Policy_Type is (LRU,        -- Least recently used
                        MRU,        -- Most recently used
                        FIFO,       -- First-in, first-out
                        PLRU        -- Pseudo-LRU
   );

   function Create_Cache(mem           : access Memory_Type'Class;
                         line_count    : Positive := 1;
                         line_size     : Positive := 8;
                         associativity : Positive := 1;
                         latency       : Time_Type := 1;
                         policy        : Policy_Type := LRU;
                         write_back    : Boolean := True)
                         return Cache_Pointer;

   function Random_Cache(next       : access Memory_Type'Class;
                         generator  : Distribution_Type;
                         max_cost   : Cost_Type)
                         return Memory_Pointer;

   overriding
   function Clone(mem : Cache_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Cache_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   procedure Reset(mem     : in out Cache_Type;
                   context : in Natural);

   overriding
   procedure Read(mem      : in out Cache_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Cache_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   function To_String(mem : Cache_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Cache_Type) return Cost_Type;

   overriding
   procedure Generate(mem  : in Cache_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   overriding
   procedure Adjust(mem : in out Cache_Type);

   overriding
   procedure Finalize(mem : in out Cache_Type);

   function Get_Line_Size(mem : Cache_Type) return Positive;

   function Get_Line_Count(mem : Cache_Type) return Positive;

   function Get_Associativity(mem : Cache_Type) return Positive;

   function Get_Policy(mem : Cache_Type) return Policy_Type;

private

   type Cache_Data is record
      address  : Address_Type := Address_Type'Last;
      age      : Long_Integer := 0;
      dirty    : Boolean      := False;
   end record;

   type Cache_Data_Pointer is access Cache_Data;

   package Cache_Vectors is new Vectors(Natural, Cache_Data_Pointer);

   type Cache_Type is new Container_Type with record
      line_size      : Positive := 8;
      line_count     : Positive := 1;
      associativity  : Positive := 1;
      latency        : Time_Type := 1;
      data           : Cache_Vectors.Vector;
      policy         : Policy_Type := LRU;
      write_back     : Boolean := True;
   end record;

end Memory.Cache;
