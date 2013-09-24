
package Memory.Container is

   type Container_Type is abstract new Memory_Type with private;

   type Container_Pointer is access all Container_Type'Class;

   function Get_Memory(mem : Container_Type'Class) return Memory_Pointer;

   procedure Set_Memory(mem   : in out Container_Type'Class;
                        other : access Memory_Type'Class);

   overriding
   function Done(mem : Container_Type) return Boolean;

   overriding
   procedure Reset(mem     : in out Container_Type;
                   context : in Natural);

   overriding
   procedure Set_Port(mem     : in out Container_Type;
                      port    : in Natural;
                      ready   : out Boolean);

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

   procedure Start(mem : in out Container_Type'Class);

   procedure Commit(mem    : in out Container_Type'Class;
                    cycles : out Time_Type);

   procedure Do_Read(mem      : in out Container_Type'Class;
                     address  : in Address_Type;
                     size     : in Positive);

   procedure Do_Write(mem     : in out Container_Type'Class;
                      address : in Address_Type;
                      size    : in Positive);

   procedure Do_Idle(mem      : in out Container_Type'Class;
                     cycles   : in Time_Type);

   overriding
   function Get_Path_Length(mem : Container_Type) return Natural;

   overriding
   procedure Show_Access_Stats(mem : in out Container_Type);

   overriding
   function To_String(mem : Container_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Container_Type) return Cost_Type;

   overriding
   function Get_Writes(mem : Container_Type) return Long_Integer;

   overriding
   function Get_Word_Size(mem : Container_Type) return Positive;

   overriding
   function Get_Ports(mem : Container_Type) return Port_Vector_Type;

   overriding
   procedure Adjust(mem : in out Container_Type);

   overriding
   procedure Finalize(mem : in out Container_Type);

private

   type Container_Type is abstract new Memory_Type with record
      mem         : access Memory_Type'Class := null;
      start_time  : Time_Type                := 0;
   end record;

end Memory.Container;
