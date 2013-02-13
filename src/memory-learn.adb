
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Generic_Elementary_Functions;

package body Memory.Learn is

   package FR renames Ada.Numerics.Float_Random;

   package Float_Funcs is
      new Ada.Numerics.Generic_Elementary_Functions(Float);

   type Value_Array is array (1 .. Bit_Count) of Float;

   procedure Evaluate_Node(node     : in out Node_Type;
                           values   : in Value_Array) is
      sum : Float := node.bias;
   begin
      for i in Value_Array'Range loop
         sum := sum + values(i) * node.weights(i);
      end loop;
      node.value := 1.0 / (1.0 + Float_Funcs.Exp(-sum));
   end Evaluate_Node;

   procedure Evaluate_Layer(layer   : in out Layer_Type;
                            ins     : in Value_Array) is
   begin
      for i in Layer_Type'Range loop
         Evaluate_Node(layer(i), ins);
      end loop;
   end Evaluate_Layer;

   function Get_Values(layer : Layer_Type) return Value_Array is
      result : Value_Array;
   begin
      for i in Layer_Type'Range loop
         result(i) := layer(i).value;
      end loop;
      return result;
   end Get_Values;

   function Get_Address(layer : Layer_Type) return Address_Type is
      result : Address_Type := 0;
   begin
      for i in Layer_Type'Range loop
         if layer(i).value > 0.5 then
            result := result or (2 ** (i - Layer_Type'First));
         end if;
      end loop;
      return result;
   end Get_Address;

   function Get_Inputs(address : Address_Type) return Value_Array is
      result : Value_Array;
   begin
      for i in Value_Array'Range loop
         if (address and (2 ** (i - Value_Array'First))) /= 0 then
            result(i) := 1.0;
         else
            result(i) := 0.0;
         end if;
      end loop;
      return result;
   end Get_Inputs;

   procedure Evaluate_Network(network  : in out Network_Type;
                              address  : in Address_Type) is
      values : Value_Array := Get_Inputs(address);
   begin
      for i in Network_Type'Range loop
         Evaluate_Layer(network(i), values);
         values := Get_Values(network(i));
      end loop;
   end Evaluate_Network;

   procedure Initialize_Network(network : in out Network_Type) is
      generator : Ada.Numerics.Float_Random.Generator;
   begin
      for layer in network'Range loop
         for node in network(layer)'Range loop
            for i in network(layer)(node).weights'Range loop
               network(layer)(node).weights(i) :=
                  0.1 * (FR.Random(generator) - 0.5);
            end loop;
         end loop;
      end loop;
      Evaluate_Network(network, 0);
   end Initialize_Network;

   procedure Get_Expected(network   : in out Network_Type;
                          address   : in Address_Type;
                          result    : out Address_Type) is
   begin
      Evaluate_Network(network, address);
      result := Get_Address(network(Network_Type'First));
   end Get_Expected;

   procedure Update_Network(network    : in out Network_Type;
                            rate       : in Float;
                            input      : in Address_Type;
                            actual     : in Address_Type) is

      inputs   : constant Value_Array := Get_Inputs(input);
      outputs  : constant Value_Array := Get_Inputs(actual);
      change   : Value_Array;
      update   : Value_Array;

   begin

      -- Get deltas for the output layer.
      for i in Layer_Type'Range loop
         declare
            -- a is the computed output.
            -- y is the true output.
            a : constant Float := network(Network_Type'Last)(i).value;
            y : constant Float := outputs(i);
         begin
            change(i) := a * (1.0 - a) * (y - a);
         end;
      end loop;

      -- Propagate error through the network.
      for layer in reverse Network_Type'First .. Network_Type'Last loop
         for j in Layer_Type'Range loop
            declare
               a     : constant Float := network(layer)(j).value;
               sum   : Float := 0.0;
            begin
               for i in Layer_Type'Range loop
                  sum := sum + network(layer)(j).weights(i) * change(i);
               end loop;
               update(j) := sum * a * (1.0 - a);
            end;
            for i in Layer_Type'Range loop
               declare
                  wji   : constant Float := network(layer)(j).weights(i);
                  dw    : Float;
               begin
                  if layer = Network_Type'First then
                     dw := rate * change(j) * inputs(i);
                  else
                     dw := rate * change(j) * network(layer - 1)(i).value;
                  end if;
                  network(layer)(j).weights(i) := wji + dw;
               end;
            end loop;
         end loop;
         change := update;
      end loop;

   end Update_Network;

   function Create_Learn(mem : access Memory_Type'Class)
                         return Learn_Pointer is
      result : constant Learn_Pointer := new Learn_Type;
   begin
      Set_Memory(result.all, mem);
      Initialize_Network(result.network);
      return result;
   end Create_Learn;

   procedure Update_Parameters(mem     : in out Learn_Type;
                               address : in Address_Type) is
   begin

      mem.total := mem.total + 1;
      if mem.expected = address then
         mem.correct := mem.correct + 1;
      end if;
      Put_Line(Address_Type'Image(mem.expected) & " vs " &
               Address_Type'Image(address));
      if mem.total = 1000 then
         Put(Long_Integer'Image(mem.correct) & " /" &
             Long_Integer'Image(mem.total) & " ->");
         declare
            temp : constant Integer
               := Integer(100.0 * Float(mem.correct) / Float(mem.total));
         begin
            Put(Integer'Image(temp));
         end;
         New_Line;
         mem.total := 0;
         mem.correct := 0;
      end if;

      Update_Network(mem.network, mem.rate, mem.last, address);
      Get_Expected(mem.network, address, mem.expected);
      mem.last := address;

   end Update_Parameters;

   procedure Read(mem      : in out Learn_Type;
                  address  : in Address_Type;
                  size     : in Positive) is
   begin
      Read(Container_Type(mem), address, size);
      Update_Parameters(mem, address);
   end Read;

   procedure Write(mem     : in out Learn_Type;
                   address : in Address_Type;
                   size    : in Positive) is
   begin
      Write(Container_Type(mem), address, size);
      Update_Parameters(mem, address);
   end Write;

   procedure Show_Access_Stats(mem : in Learn_Type) is
   begin
      Show_Access_Stats(Container_Type(mem));
   end Show_Access_Stats;

end Memory.Learn;
