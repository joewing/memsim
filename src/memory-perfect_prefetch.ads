
package Memory.Perfect_Prefetch is

   type Perfect_Prefetch_Type is new Memory_Type with private;

   type Perfect_Prefetch_Pointer is access all Perfect_Prefetch_Type'Class;

   function Create_Perfect_Prefetch(mem   : access Memory_Type'Class)
                                    return Perfect_Prefetch_Pointer;

   overriding
   procedure Read(mem      : in out Perfect_Prefetch_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Perfect_Prefetch_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Perfect_Prefetch_Type;
                  cycles   : in Time_Type);

   overriding
   procedure Show_Access_Stats(mem : in out Perfect_Prefetch_Type);

   overriding
   function To_String(mem : Perfect_Prefetch_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Perfect_Prefetch_Type) return Natural;

   overriding
   procedure Finalize(mem : in out Perfect_Prefetch_Type);

private

   type Perfect_Prefetch_Type is new Memory_Type with record
      mem         : access Memory_Type'Class;
      pending     : Time_Type := 0;
   end record;

end Memory.Perfect_Prefetch;
