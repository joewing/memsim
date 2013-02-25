
function Random_Enum(value : Natural) return Enum_Type is
   first : constant Natural := Enum_Type'Pos(Enum_Type'First);
   last  : constant Natural := Enum_Type'Pos(Enum_Type'Last);
   count : constant Natural := last - first + 1;
   rand  : constant Natural := value mod count;
begin
   return Enum_Type'Val(first + rand);
end Random_Enum;
