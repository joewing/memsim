
package body Variance is

   procedure Reset(v : in out Variance_Type) is
   begin
      v.sample_count := 0;
      v.sum := 0.0;
      v.sum_squares := 0.0;
   end Reset;

   procedure Update(v      : in out Variance_Type;
                    value  : in T) is
   begin
      v.sample_count := v.sample_count + 1;
      v.sum          := v.sum + value;
      v.sum_squares  := v.sum_squares + value * value;
   end Update;

   function Get_Variance(v : Variance_Type) return T is
      count : constant T := T(v.sample_count);
      mean  : constant T := v.sum / count;
   begin
      if v.sample_count > 0 then
         return v.sum_squares / count - mean * mean;
      else
         return 0.0;
      end if;
   end Get_Variance;

end Variance;
