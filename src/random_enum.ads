
-- Select an enumeration value uniformly at random.
generic
   type Enum_Type is (<>);
function Random_Enum(value : Natural) return Enum_Type;
