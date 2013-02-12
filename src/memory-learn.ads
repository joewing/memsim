
with Memory.Container; use Memory.Container;

package Memory.Learn is

   type Learn_Type is new Container_Type with private;

   type Learn_Pointer is access all Learn_Type'Class;

   function Create_Learn(mem : access Memory_Type'Class) return Learn_Pointer;

   overriding
   procedure Read(mem      : in out Learn_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out Learn_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   procedure Show_Access_Stats(mem : in Learn_Type);

private

   Bit_Count : constant := Address_Type'Size;

   type Weight_Array is array(1 .. Bit_Count) of Float;

   type Node_Type is record
      weights  : Weight_Array;
      bias     : Float := 0.0;
      value    : Float := 0.0;
   end record;

   type Layer_Type is array (1 .. Bit_Count) of Node_Type;

   type Network_Type is array (1 .. 2) of Layer_Type;

   type Learn_Type is new Container_Type with record
      expected    : Address_Type := 0;
      last        : Address_Type := 0;
      network     : Network_Type;
      rate        : Float := 0.1;
      correct     : Long_Integer := 0;
      total       : Long_Integer := 0;
   end record;

end Memory.Learn;
