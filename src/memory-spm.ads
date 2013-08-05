
with Memory.Container; use Memory.Container;

package Memory.SPM is

   type SPM_Type is new Container_Type with private;

   type SPM_Pointer is access all SPM_Type'Class;

   function Create_SPM(mem       : access Memory_Type'Class;
                       size      : Natural;
                       latency   : Time_Type := 1) return SPM_Pointer;

   function Random_SPM(next      : access Memory_Type'Class;
                       generator : Distribution_Type;
                       max_cost  : Cost_Type)
                       return Memory_Pointer;

   overriding
   function Clone(mem : SPM_Type) return Memory_Pointer;

   overriding
   procedure Permute(mem         : in out SPM_Type;
                     generator   : in Distribution_Type;
                     max_cost    : in Cost_Type);

   overriding
   procedure Read(mem      : in out SPM_Type;
                  address  : in Address_Type;
                  size     : in Positive);

   overriding
   procedure Write(mem     : in out SPM_Type;
                   address : in Address_Type;
                   size    : in Positive);

   overriding
   function Get_Path_Length(mem : SPM_Type) return Natural;

   overriding
   function To_String(mem : SPM_Type) return Unbounded_String;

   overriding
   function Get_Cost(mem : SPM_Type) return Cost_Type;

   overriding
   procedure Generate(mem  : in SPM_Type;
                      sigs : in out Unbounded_String;
                      code : in out Unbounded_String);

   function Get_Size(mem : SPM_Type) return Natural;

private

   type SPM_Type is new Container_Type with record
      size     : Natural;
      latency  : Time_Type;
   end record;

end Memory.SPM;
