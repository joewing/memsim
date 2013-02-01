
package Memory.Prefetch is

   type Prefetch_Type is new Memory_Type with private;

   type Prefetch_Pointer is access all Prefetch_Type'class;

   function Create_Prefetch(mem        : access Memory_Type'class;
                            stride     : Address_Type := 1;
                            multiplier : Address_Type := 1)
                            return Prefetch_Pointer;

   overriding
   procedure Read(mem      : in out Prefetch_Type;
                  address  : in Address_Type);

   overriding
   procedure Write(mem     : in out Prefetch_Type;
                   address : in Address_Type);

   overriding
   procedure Idle(mem      : in out Prefetch_Type;
                  cycles   : in Time_Type);

private

   type Prefetch_Type is new Memory_Type with record
      mem         : access Memory_Type'class;
      pending     : Time_Type := 0;
      stride      : Address_Type := 1;
      multiplier  : Address_Type := 1;
   end record;

end Memory.Prefetch;
