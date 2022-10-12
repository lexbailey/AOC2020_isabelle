section "Solution to Day 1 of AoC 2020"

theory day1
  imports Main "HOL.Code_Numeral" string_utils
begin

text "This is a solution to the puzzle for day 1"

subsection "Input parsing"

definition parse_input :: "string \<Rightarrow> natural list"
  where "parse_input s = map str_to_nat (split CHR ''\<newline>'' (trim s))"

subsection "Solution Algorithm"

text "prod\\_of\\_sum takes a target number and a list of input numbers, finds the first pair that
sums to the target number, and returns the product of those two numbers"

fun prod_of_sum :: "natural \<Rightarrow> natural list \<Rightarrow> natural"
  where "prod_of_sum t (Cons h rest) = (if (List.member rest (t - h)) then (h * (t - h)) else (prod_of_sum t rest))"
  |"prod_of_sum t Nil = 0"

text "The solution to part 1 is simply $$(@{const prod_of_sum} 2020)$$ applied to the input list"

fun part1 :: "String.string \<Rightarrow> natural"
  where "part1 a = (prod_of_sum 2020 (parse_input a))"

text "For the second part we need to do a little bit more checking, we now need three different
numbers from the list to add to the target value of 2020"

fun prod3_of_sum :: "natural \<Rightarrow> natural list \<Rightarrow> natural"
  where "prod3_of_sum t (Cons h rest) =
    (let p = (h * (prod_of_sum (t - h) rest)) in
        (if (p = 0) then (prod3_of_sum t rest) else p)
    ) 
   "
  |"prod3_of_sum t Nil = 0"

fun part2 :: "String.string \<Rightarrow> natural"
  where "part2 s = prod3_of_sum 2020 (parse_input s)"

subsection "Testing"

text "We expect our test case to return 514579"

definition example_input :: "string" where "example_input = ''1721
979
366
299
675
1456
''"

lemma "part1 example_input = 514579"
  by eval

text "We expect our test case for part 2 to return 241861950"

lemma "part2 example_input = 241861950"
  by eval

export_code "part1" "part2" in Haskell module_name Solution

end
