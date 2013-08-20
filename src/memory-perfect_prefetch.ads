
with Memory.Container; use Memory.Container;

package Memory.Perfect_Prefetch is

   type Perfect_Prefetch_Type is new Container_Type with private;

   type Perfect_Prefetch_Pointer is access all Perfect_Prefetch_Type'Class;

   function Create_Perfect_Prefetch(mem   : access Memory_Type'Class)
                                    return Perfect_Prefetch_Pointer;

   overriding
   function Clone(mem : Perfect_Prefetch_Type) return Memory_Pointer;

   overriding
   procedure Reset(mem     : in out Perfect_Prefetch_Type;
                   context : in Natural);

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
   function To_String(mem : Perfect_Prefetch_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : Perfect_Prefetch_Type) return Cost_Type;

   overriding
   procedure Generate(mem  : in Perfect_Prefetch_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

private

   type Perfect_Prefetch_Type is new Container_Type with record
      pending     : Time_Type := 0;
   end record;

end Memory.Perfect_Prefetch;
