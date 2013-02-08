
package Memory.Transform is

   type Transform_Type is abstract new Memory_Type with private;

   overriding
   procedure Start(mem : in out Transform_Type);

   overriding
   procedure Commit(mem    : in out Transform_Type;
                    cycles : out Time_Type);

   overriding
   procedure Read(mem      : in out Transform_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Transform_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Transform_Type;
                  cycles   : in Time_Type);

   function Apply(mem      : Transform_Type;
                  address  : Address_Type)
                  return Address_Type is abstract;

private

   type Transform_Type is abstract new Memory_Type with record
      mem   : access Memory_Type'Class;
   end record;

   overriding
   procedure Show_Access_Stats(mem : in Transform_Type);

   overriding
   procedure Finalize(mem : in out Transform_Type);

end Memory.Transform;
