
with Ada.Containers.Vectors;
with Memory.Container; use Memory.Container;

package Memory.Super is

   type Super_Type is new Container_Type with private;

   type Super_Pointer is access all Super_Type'Class;

   function Create_Super(max_cost   : Cost_Type;
                         dram       : not null access Memory_Type'Class)
                         return Super_Pointer;

   overriding
   function Clone(mem : Super_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem : in out Super_Type);

   overriding
   procedure  Read(mem     : in out Super_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Write(mem     : in out Super_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Super_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Access_Stats(mem : in out Super_Type);

   overriding
   procedure Adjust(mem : in out Super_Type);

   overriding
   procedure Finalize(mem : in out Super_Type);

private

   package Memory_Vectors is new Ada.Containers.Vectors(Natural,
                                                        Container_Pointer);

   type Super_Type is new Container_Type with record
      max_cost       : Cost_Type             := 1e6;
      dram           : Memory_Pointer        := null;
      generator      : Generator_Pointer     := new RNG.Generator;
      best_name      : Unbounded_String      := Null_Unbounded_String;
      best_time      : Time_Type             := Time_Type'Last;
      best_cost      : Cost_Type             := Cost_Type'Last;
      chain          : Memory_Vectors.Vector;
      last_chain     : Memory_Vectors.Vector;
      last_cost      : Cost_Type             := Cost_Type'Last;
      last_time      : Time_Type             := Time_Type'Last;
   end record;

end Memory.Super;
