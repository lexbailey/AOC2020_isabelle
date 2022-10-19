section "Solution to Day N of AoC 2020"

theory dayN
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils list_utils
begin

text "This is a solution to the puzzle for day N"

subsection "Input parsing"

(* TODO insert input parsing code here *)

subsection "Solution Algorithm"

(* TODO replace part1 and part2 below with real solutions *)

definition part1 :: "string \<Rightarrow> natural"
  where "part1 s = 0"

definition part2 :: "string \<Rightarrow> natural"
  where "part2 s = 0"

subsection "Testing"

(* TODO evaluate with some example inputs *)

definition example_input::string where "example_input = ''
''"

lemma "part1 example_input = 0"
  by eval

lemma "part2 example_input = 0"
  by eval

(* TODO remember to export part2 once complete *)

export_code "part1" in Haskell module_name Solution

end
