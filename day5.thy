section "Solution to Day 5 of AoC 2020"

theory day5
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils list_utils
begin

text "This is a solution to the puzzle for day 5"

subsection "Input parsing"

text "the inputs are numbers, but encoded strangely. Just binary numbers where the 1s are either
R or B and the 0s are either F or L"

definition convert_digit :: "char \<Rightarrow> char"
  where "convert_digit c = (case c of
    CHR ''R'' \<Rightarrow> CHR ''1''
    |CHR ''B'' \<Rightarrow> CHR ''1''
    |c\<Rightarrow> CHR ''0''
  )"

definition parse_line :: "string \<Rightarrow> natural"
  where "parse_line = str_to_nat_base 2 \<circ> (map convert_digit)"

definition parse_input :: "string \<Rightarrow> natural list"
  where "parse_input = map parse_line \<circ> split CHR ''\<newline>'' \<circ> trim"

subsection "Solution Algorithm"

text "The first part of this one's trivial, just find the max number"

definition list_max :: "natural list \<Rightarrow> natural"
  where "list_max = (reduce max 0)"

definition part1 :: "string \<Rightarrow> natural"
  where "part1 = list_max \<circ> parse_input"

text "the second part is a little more interesting, find a number N in the range 0 to 1023 which is
not in the list, but where N+1 and N-1 are in the list"

definition contains :: "'a \<Rightarrow> 'a list \<Rightarrow> bool"
  where "contains item l = reduce (\<or>) False (map ((=) item) l)"

definition is_hole :: "natural \<Rightarrow> natural list \<Rightarrow> bool"
  where "is_hole n l = (
    (contains (n-1) l)
    \<and> (contains (n+1) l)
    \<and> (\<not>contains n l)
  )"

definition part2 :: "string \<Rightarrow> natural"
  where "part2 a = hd (let l = (parse_input a) in filter (\<lambda>b. is_hole b l) [0...<1024])"

subsection "Testing"

text "We expect our test case to return 820"

definition example_input::string where "example_input = ''
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL
''"

value "part1 example_input"

text "For part 2 there was no example provided, so I wrote an example input, we expect 820 again."

definition possible_input_2 :: string where "possible_input_2 = ''
BFFFBBFLLL
BBFFBBFLLR
BBFFBBFLRL
BBFFBBFLRR
BBFFBBFRLR
BBFFBBFRRL
BBFFBBFRRR
''"

(* 1100110100 *)

value "part2 possible_input_2"

export_code "part1" "part2" in Haskell module_name Solution

end
