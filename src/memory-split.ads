
with Memory.Container; use Memory.Container;

package Memory.Split is

   type Split_Type is new Container.Container_Type with private;

   type Split_Pointer is access all Split_Type'Class;

   function Create_Split return Split_Pointer;

   function Random_Split(next       : access Memory_Type'Class;
                         generator  : RNG.Generator;
                         max_cost   : Cost_Type)
                         return Memory_Pointer;

   overriding
   function Clone(mem : Split_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Split_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   function Get_Bank(mem    : Split_Pointer;
                     index  : Natural) return Memory_Pointer;

   procedure Set_Bank(mem   : in Split_Pointer;
                      index : in Natural;
                      other : access Memory_Type'Class);

   function Get_Offset(mem : Split_Type'Class) return Address_Type;

   procedure Set_Offset(mem      : in out Split_Type'Class;
                        offset   : in Address_Type);

   overriding
   procedure Reset(mem : in out Split_Type);

   overriding
   procedure Read(mem      : in out Split_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Split_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Split_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Access_Stats(mem : in out Split_Type);

   overriding
   function To_String(mem : Split_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Split_Type) return Cost_Type;

   overriding
   function Get_Writes(mem : Split_Type) return Long_Integer;

   overriding
   procedure Generate(mem  : in Split_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   overriding
   procedure Adjust(mem : in out Split_Type);

   overriding
   procedure Finalize(mem : in out Split_Type);

private

   type Split_Data is record
      mem      : access Memory_Type'Class;
   end record;

   type Split_Data_Array is array(0 .. 1) of Split_Data;

   type Split_Type is new Container.Container_Type with record
      banks    : Split_Data_Array;
      offset   : Address_Type;
   end record;

end Memory.Split;
