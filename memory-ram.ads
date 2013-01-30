
package Memory.RAM is

   type RAM is new Memory_Base with private;

   type RAM_Pointer is access all RAM'class;

   procedure Read(mem      : access RAM;
                  address  : Address_Type);

   procedure Write(mem     : access RAM;
                   address : Address_Type);

   procedure Set_Latency(mem     : access RAM;
                         latency : Natural);

private

   type RAM is new Memory_Base with record
      latency  : Natural := 1;
   end record;

end Memory.RAM;
