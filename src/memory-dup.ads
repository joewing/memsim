
package Memory.Dup is

   type Dup_Type is new Memory_Type with private;

   type Dup_Pointer is access all Dup_Type'Class;

   function Create_Dup return Dup_Pointer;

   procedure Add_Memory(mem   : in out Dup_Type;
                        other : access Memory_Type'Class);

   overriding
   procedure Start(mem : in out Dup_Type);

   overriding
   procedure Commit(mem    : in out Dup_Type;
                    cycles : out Time_Type);

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
   procedure Show_Stats(mem : in Dup_Type);

   overriding
   function To_String(mem : Dup_Type) return Unbounded_String;

   overriding
   procedure Finalize(mem : in out Dup_Type);

private

   package Memory_Vectors is new Vectors(Natural, Memory_Pointer);

   type Dup_Type is new Memory_Type with record
      memories : Memory_Vectors.Vector;
   end record;

end Memory.Dup;
