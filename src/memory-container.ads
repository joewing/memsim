
package Memory.Container is

   type Container_Type is abstract new Memory_Type with private;

   procedure Set_Memory(mem   : in out Container_Type'Class;
                        other : access Memory_Type'Class);

   overriding
   procedure Start(mem : in out Container_Type);

   overriding
   procedure Commit(mem    : in out Container_Type;
                    cycles : out Time_Type);

   overriding
   procedure Read(mem      : in out Container_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Container_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Container_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Access_Stats(mem : in Container_Type);

   overriding
   procedure Finalize(mem : in out Container_Type);

private

   type Container_Type is abstract new Memory_Type with record
      mem            : access Memory_Type'Class := null;
   end record;

end Memory.Container;
