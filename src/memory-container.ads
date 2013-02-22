
package Memory.Container is

   type Container_Type is abstract new Memory_Type with private;

   type Container_Pointer is access all Container_Type'Class;

   procedure Set_Memory(mem   : in out Container_Type'Class;
                        other : access Memory_Type'Class);

   function Get_Memory(mem : Container_Type'Class) return Memory_Pointer;

   overriding
   procedure Reset(mem : in out Container_Type);

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

   procedure Forward_Start(mem : in out Container_Type'Class);

   procedure Forward_Commit(mem     : in out Container_Type'Class;
                            cycles  : out Time_Type);

   procedure Forward_Read(mem       : in out Container_Type'Class;
                          address   : in Address_Type;
                          size      : in Positive);

   procedure Forward_Write(mem      : in out Container_Type'Class;
                           address  : in Address_Type;
                           size     : in Positive);

   procedure Forward_Idle(mem       : in out Container_Type'Class;
                          cycles    : in Time_Type);

   overriding
   procedure Show_Access_Stats(mem : in out Container_Type);

   overriding
   function To_String(mem : Container_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Container_Type) return Cost_Type;

   overriding
   procedure Adjust(mem : in out Container_Type);

   overriding
   procedure Finalize(mem : in out Container_Type);

private

   type Container_Type is abstract new Memory_Type with record
      mem : access Memory_Type'Class := null;
   end record;

end Memory.Container;
