
with Memory.Container; use Memory.Container;

package Memory.Trace is

   type Trace_Type is new Container_Type with private;

   type Trace_Pointer is access all Trace_Type'Class;

   function Create_Trace(mem : Memory_Pointer) return Trace_Pointer;

   overriding
   function Clone(mem : Trace_Type) return Memory_Pointer;

   overriding
   procedure Read(mem      : in out Trace_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Trace_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Trace_Type;
                  cycles   : in Time_Type);

private

   type Trace_Type is new Container_Type with null record;

end Memory.Trace;
