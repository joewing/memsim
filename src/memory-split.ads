
with Memory.Wrapper; use Memory.Wrapper;

package Memory.Split is

   type Split_Type is new Wrapper_Type with private;

   type Split_Pointer is access all Split_Type'Class;

   function Create_Split return Split_Pointer;

   function Random_Split(next       : access Memory_Type'Class;
                         generator  : Distribution_Type;
                         max_cost   : Cost_Type)
                         return Memory_Pointer;

   overriding
   function Clone(mem : Split_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out Split_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   function Get_Bank(mem    : Split_Type;
                     index  : Natural) return Memory_Pointer;

   procedure Set_Bank(mem   : in out Split_Type;
                      index : in Natural;
                      other : access Memory_Type'Class);

   function Get_Offset(mem : Split_Type'Class) return Address_Type;

   procedure Set_Offset(mem      : in out Split_Type'Class;
                        offset   : in Address_Type);

   overriding
   procedure Reset(mem     : in out Split_Type;
                   context : in Natural);

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
   function Get_Path_Length(mem : Split_Type) return Natural;

   overriding
   procedure Generate(mem  : in Split_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   overriding
   procedure Adjust(mem : in out Split_Type);

   overriding
   procedure Finalize(mem : in out Split_Type);

   overriding
   procedure Forward_Read(mem       : in out Split_Type;
                          source    : in Natural;
                          address   : in Address_Type;
                          size      : in Positive);

   overriding
   procedure Forward_Write(mem      : in out Split_Type;
                           source   : in Natural;
                           address  : in Address_Type;
                           size     : in Positive);

   overriding
   procedure Forward_Idle(mem    : in out Split_Type;
                          source : in Natural;
                          cycles : in Time_Type);

   overriding
   function Forward_Get_Time(mem : Split_Type) return Time_Type;

   overriding
   function Get_Join_Length(mem : Split_Type) return Natural;

private

   type Split_Data is record
      mem      : access Memory_Type'Class;
   end record;

   type Split_Data_Array is array(0 .. 1) of Split_Data;

   type Split_Type is new Wrapper_Type with record
      banks    : Split_Data_Array;
      offset   : Address_Type;
   end record;

end Memory.Split;
