
package Memory.Prefetch is

   type Prefetch_Type is new Memory_Type with private;

   type Prefetch_Pointer is access all Prefetch_Type'class;

   function Create_Prefetch(mem     : access Memory_Type'class;
                            stride  : Integer := 1) return Prefetch_Pointer;

   overriding
   procedure Read(mem      : in out Prefetch_Type;
                  address  : Address_Type);

   overriding
   procedure Write(mem     : in out Prefetch_Type;
                   address : Address_Type);

private

   type Prefetch_Type is new Memory_Type with record
      mem      : access Memory_Type'class;
      stride   : Integer := 1;
   end record;

end Memory.Prefetch;
