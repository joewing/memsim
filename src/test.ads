
with GNAT.Source_Info;

with Memory.Container; use Memory.Container;
use Memory;

package Test is

   procedure Run_Tests;

private

   type Monitor_Type is new Container_Type with record
      reads    : Natural   := 0;
      writes   : Natural   := 0;
      cycles   : Time_Type := 0;
   end record;

   type Monitor_Pointer is access all Monitor_Type;

   overriding
   function Clone(mem : Monitor_Type) return Memory_Pointer;

   overriding
   procedure Read(mem      : in out Monitor_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Monitor_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Idle(mem      : in out Monitor_Type;
                  cycles   : in Time_Type);

   procedure Check(cond    : in Boolean;
                   source  : in String := GNAT.Source_Info.File;
                   line    : in Natural := GNAT.Source_Info.Line);

   count    : Natural := 0;
   failed   : Natural := 0;

end Test;
