
with Memory.Container; use Memory.Container;
with Ada.Containers.Vectors;

package Memory.Arbiter is

   type Arbiter_Type is new Container_Type with private;

   type Arbiter_Pointer is access all Arbiter_Type'Class;

   function Create_Arbiter(next : access Memory_Type'Class)
                           return Arbiter_Pointer;

   overriding
   function Clone(mem : Arbiter_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem     : in out Arbiter_Type;
                   context : in Natural);

   overriding
   procedure Set_Port(mem     : in out Arbiter_Type;
                      port    : in Natural;
                      ready   : out Boolean);

   overriding
   procedure Read(mem      : in out Arbiter_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Arbiter_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Arbiter_Type;
                  cycles   : in Time_Type);

   overriding
   function To_String(mem : Arbiter_Type) return Unbounded_String;

private

   package Pending_Vectors is new Vectors(Natural, Time_Type);

   type Arbiter_Type is new Container_Type with record

      -- The current port.
      port     : Natural := 0;

      -- Keep track of the earliest time the next even can happen for
      -- each port.
      pending  : Pending_Vectors.Vector;

   end record;

end Memory.Arbiter;
