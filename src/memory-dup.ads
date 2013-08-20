
package Memory.Dup is

   type Dup_Type is new Memory_Type with private;

   type Dup_Pointer is access all Dup_Type'Class;

   function Create_Dup return Dup_Pointer;

   overriding
   function Clone(mem : Dup_Type) return Memory_Pointer;

   procedure Add_Memory(mem   : in out Dup_Type;
                        other : access Memory_Type'Class);

   overriding
   procedure Reset(mem     : in out Dup_Type;
                   context : in Natural);

   overriding
   procedure Read(mem      : in out Dup_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Dup_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Dup_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Stats(mem : in out Dup_Type);

   overriding
   function To_String(mem : Dup_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Dup_Type) return Cost_Type;

   overriding
   function Get_Writes(mem : Dup_Type) return Long_Integer;

   overriding
   function Get_Word_Size(mem : Dup_Type) return Positive;

   overriding
   procedure Generate(mem  : in Dup_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   overriding
   function Get_Ports(mem : Dup_Type) return Port_Vector_Type;

   overriding
   procedure Adjust(mem : in out Dup_Type);

   overriding
   procedure Finalize(mem : in out Dup_Type);

private

   package Memory_Vectors is new Vectors(Natural, Memory_Pointer);

   type Dup_Type is new Memory_Type with record
      memories : Memory_Vectors.Vector;
   end record;

end Memory.Dup;
