
with Memory.Container; use Memory.Container;

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
   procedure Set_Port(mem  : in out Arbiter_Type;
                      port : in Natural);

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

   type Arbiter_Type is new Container_Type with null record;

end Memory.Arbiter;
