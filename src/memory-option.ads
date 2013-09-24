
package Memory.Option is

   type Option_Type is new Memory_Type with private;

   type Option_Pointer is access all Option_Type'Class;

   function Create_Option return Option_Pointer;

   function Clone(mem : Option_Type) return Memory_Pointer;

   procedure Permute(mem         : in out Option_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   procedure Add_Memory(mem   : in out Option_Type;
                        other : access Memory_Type'Class);

   overriding
   function Done(mem : Option_Type) return Boolean;

   overriding
   procedure Reset(mem     : in out Option_Type;
                   context : in Natural);

   overriding
   procedure Set_Port(mem     : in out Option_Type;
                      port    : in Natural;
                      ready   : out Boolean);

   overriding
   procedure Read(mem      : in out Option_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Option_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Option_Type;
                  cycles   : in Time_Type);

   overriding
   function Get_Time(mem : Option_Type) return Time_Type;

   overriding
   function Get_Writes(mem : Option_Type) return Long_Integer;

   overriding
   function To_String(mem : Option_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Option_Type) return Cost_Type;

   overriding
   function Get_Word_Size(mem : Option_Type) return Positive;

   overriding
   procedure Generate(mem  : in Option_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   overriding
   function Get_Ports(mem : Option_Type) return Port_Vector_Type;

private

   package Memory_Vectors is new Vectors(Natural, Memory_Pointer);

   subtype Memory_Vector_Type is Memory_Vectors.Vector;

   type Option_Type is new Memory_Type with record
      memories : Memory_Vector_Type;
      index    : Natural := 0;
   end record;

   overriding
   procedure Adjust(mem : in out Option_Type);

   overriding
   procedure Finalize(mem : in out Option_Type);

end Memory.Option;
