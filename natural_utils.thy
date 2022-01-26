section "Utils for HOL.Code\\_Numeral natural type"
theory natural_utils
imports Main HOL.Code_Numeral
begin

fun count_bool :: "bool \<Rightarrow> natural"
  where "count_bool True = 1"
  | "count_bool False = 0"

end