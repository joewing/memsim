
with Ada.Finalization; use Ada.Finalization;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;

with Distribution; use Distribution;
with Util; use Util;

-- Base package for memory components.
-- This package has prototypes for simulating and generating VHDL.
package Memory is

   -- An exception that is raised when the simulation of a memory
   -- can complete early.  This is used for superoptimization to avoid
   -- fully evaluating a memory trace.
   Prune_Error : exception;

   -- The base data type for memory components.
   -- This type should be extended to create new main memories.
   -- See Memory.Container to create components.
   type Memory_Type is abstract new Controlled with private;
   type Memory_Pointer is access all Memory_Type'Class;

   -- Type to represent a port.
   type Port_Type is record
      id          : Natural;     -- Memory identifier for the port.
      word_size   : Positive;    -- Size of a word in bytes.
      addr_bits   : Positive;    -- Size of an address in bits.
   end record;

   -- Vector of ports.
   package Port_Vectors is new Vectors(Natural, Port_Type);
   subtype Port_Vector_Type is Port_Vectors.Vector;

   -- Clone this memory.
   -- This will allocate a new instance of the memory.
   function Clone(mem : Memory_Type) return Memory_Pointer is abstract;

   -- Permute some aspect of the current memory component.
   procedure Permute(mem         : in out Memory_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type) is null;

   -- Determine if the evaulation is finished or if the benchmark
   -- needs to be re-run.
   function Done(mem : Memory_Type) return Boolean;

   -- Reset the memory.  This should be called between benchmark runs.
   -- context is the context being started.  This is used for
   -- superoptimization of multiple traces.
   procedure Reset(mem     : in out Memory_Type;
                   context : in Natural);

   -- Set the port being used to access memory.
   -- This is used to inform arbiters where an access is coming from.
   procedure Set_Port(mem     : in out Memory_Type;
                      port    : in Natural;
                      ready   : out Boolean) is null;

   -- Simulate a memory read.
   procedure Read(mem      : in out Memory_Type;
                  address  : in Address_Type;
                  size     : in Positive) is abstract;

   -- Simulate a memory write.
   procedure Write(mem     : in out Memory_Type;
                   address : in Address_Type;
                   size    : in Positive) is abstract;

   -- Simulate idle time.
   procedure Idle(mem      : in out Memory_Type;
                  cycles   : in Time_Type);

   -- Get the current time in cycles.
   function Get_Time(mem : Memory_Type) return Time_Type;

   -- Get the number of writes to the main memory.
   function Get_Writes(mem : Memory_Type) return Long_Integer is abstract;

   -- Show statistics.
   procedure Show_Stats(mem : in out Memory_Type);

   -- Show access statistics (called from Show_Stats).
   procedure Show_Access_Stats(mem : in out Memory_Type) is null;

   -- Get a string representation of the memory.
   function To_String(mem : Memory_Type) return Unbounded_String is abstract;

   -- Get the cost of this memory.
   function Get_Cost(mem : Memory_Type) return Cost_Type is abstract;

   -- Get the word size in bytes.
   function Get_Word_Size(mem : Memory_Type) return Positive is abstract;

   -- Get the length of the longest path in levels of logic.
   -- This is an estimate used to insert registers.
   function Get_Path_Length(mem : Memory_Type) return Natural;

   -- Generate VHDL for a memory.
   procedure Generate(mem  : in Memory_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String) is null;

   -- Destroy a memory.
   procedure Destroy(mem : in out Memory_Pointer);

   -- Get the total access time.
   -- This is used for optimization to minimize access time.
   function Get_Time(mem : access Memory_Type'Class) return Time_Type;

   -- Get the total number of writes.
   -- This is used for optimization to minimize writes.
   function Get_Writes(mem : access Memory_Type'Class) return Long_Integer;

   -- Return 0.
   -- This is used for optimization to perform a random walk.
   function Get_Zero(mem : access Memory_Type'Class) return Natural;

   -- Get a unique identifier for this memory component.
   function Get_ID(mem : Memory_Type'Class) return Natural;

   -- Advance the access time.
   procedure Advance(mem      : in out Memory_Type'Class;
                     cycles   : in Time_Type);

   -- Get the maximum path length in levels of logic.
   function Get_Max_Length(mem      : access Memory_Type'Class;
                           result   : Natural := 0) return Natural;

   -- Get a vector of end-points.
   function Get_Ports(mem : Memory_Type) return Port_Vector_Type is abstract;

private

   type Memory_Type is abstract new Controlled with record
      id    : Natural;
      time  : Time_Type := 0;
   end record;

   overriding
   procedure Initialize(mem : in out Memory_Type);

   overriding
   procedure Adjust(mem : in out Memory_Type);

   function Get_Port(mem : Memory_Type'Class) return Port_Type;

   procedure Declare_Signals(sigs      : in out Unbounded_String;
                             name      : in String;
                             word_bits : in Positive);

   procedure Line(dest  : in out Unbounded_String;
                  str   : in String := "");

   function To_String(a : Address_Type) return String;

   function To_String(t : Time_Type) return String;

   function To_String(i : Integer) return String renames Util.To_String;

   function To_String(i : Long_Integer) return String renames Util.To_String;

   function To_String(f : Long_Float) return String renames Util.To_String;

end Memory;
