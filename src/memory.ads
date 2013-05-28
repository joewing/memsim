
with Ada.Numerics.Discrete_Random;
with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Util;

package Memory is

   package RNG is new Ada.Numerics.Discrete_Random(Natural);

   type Generator_Pointer is access all RNG.Generator;

   Prune_Error    : exception;

   procedure Destroy is new Ada.Unchecked_Deallocation(RNG.Generator,
                                                       Generator_Pointer);

   type Address_Type is mod 2 ** 64;

   type Time_Type is new Long_Integer range 0 .. Long_Integer'Last;

   type Cost_Type is new Long_Integer range 0 .. Long_Integer'Last;

   type Memory_Type is abstract new Controlled with private;

   type Memory_Pointer is access all Memory_Type'Class;

   function Clone(mem : Memory_Type) return Memory_Pointer is abstract;

   procedure Permute(mem         : in out Memory_Type;
                     generator   : in RNG.Generator;
                     max_cost    : in Cost_Type);

   procedure Reset(mem : in out Memory_Type);

   procedure Read(mem      : in out Memory_Type;
                  address  : in Address_Type;
                  size     : in Positive) is abstract;

   procedure Write(mem     : in out Memory_Type;
                   address : in Address_Type;
                   size    : in Positive) is abstract;

   procedure Idle(mem      : in out Memory_Type;
                  cycles   : in Time_Type);

   function Get_Time(mem : Memory_Type) return Time_Type;

   function Get_Writes(mem : Memory_Type) return Long_Integer is abstract;

   procedure Show_Stats(mem : in out Memory_Type);

   procedure Show_Access_Stats(mem : in out Memory_Type);

   function To_String(mem : Memory_Type) return Unbounded_String is abstract;

   function Get_Cost(mem : Memory_Type) return Cost_Type is abstract;

   function Get_Word_Size(mem : Memory_Type) return Positive is abstract;

   procedure Generate(mem  : in Memory_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is null;

   procedure Destroy(mem : in out Memory_Pointer);

   function Get_Time(mem : access Memory_Type'Class) return Time_Type;

   function Get_Writes(mem : access Memory_Type'Class) return Long_Integer;

   function Get_Zero(mem : access Memory_Type'Class) return Natural;

   function Get_ID(mem : Memory_Type'Class) return Natural;

private

   type Memory_Type is abstract new Controlled with record
      id    : Natural;
      time  : Time_Type := 0;
   end record;

   overriding
   procedure Initialize(mem : in out Memory_Type);

   overriding
   procedure Adjust(mem : in out Memory_Type);

   procedure Advance(mem      : in out Memory_Type'Class;
                     cycles   : in Time_Type);

   procedure Declare_Signals(sigs      : in out Unbounded_String;
                             name      : in String;
                             word_bits : in Positive);

   procedure Line(dest  : in out Unbounded_String;
                  str   : in String := "");

   function To_String(a : Address_Type) return String;

   function To_String(t : Time_Type) return String;

   function To_String(i : Integer) return String renames Util.To_String;

end Memory;
