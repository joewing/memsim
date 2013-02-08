
package Memory.Trace is

   type Trace_Type is new Memory_Type with private;

   type Trace_Pointer is access all Trace_Type'Class;

   function Create_Trace(mem : Memory_Pointer) return Trace_Pointer;

   overriding
   procedure Start(mem : in out Trace_Type);

   overriding
   procedure Commit(mem    : in out Trace_Type;
                    cycles : out Time_Type);

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

   type Trace_Type is new Memory_Type with record
      mem : Memory_Pointer;
   end record;

end Memory.Trace;
