
generic
   type T is digits <>;
package Variance is

   type Variance_Type is private;

   procedure Reset(v : in out Variance_Type);

   procedure Update(v      : in out Variance_Type;
                    value  : in T);

   function Get_Variance(v : Variance_Type) return T;

private

   type Variance_Type is record
      sample_count   : Long_Integer := 0;
      sum            : T            := 0.0;
      sum_squares    : T            := 0.0;
   end record;

end Variance;
