section "Solution to Day 2 of AoC 2020"

theory day2
  imports Main "HOL.Code_Numeral" string_utils natural_utils list_natural_utils
begin

text "This is a solution to the puzzle for day 2"

subsection "Input parsing"

text "Parse the policies from the file"

type_synonym policy = "(natural * natural * char)"

fun parse_line :: "string \<Rightarrow> (policy * string)"
  where "(parse_line s) = (
    (let (min, rest) = (split_once CHR ''-'' s) in
      (let (max, rest) = (split_once CHR '' '' rest) in
        (let (c, rest) = (split_once CHR '':'' rest) in
          ((str_to_nat min, str_to_nat max, list.hd c), list.tl rest)
        )
      )
    )
  )"

text "an example policy spec and string to check"
value "parse_line ''1-3 a: abcde''"

text "parse\\_input parses the whole file"
fun parse_input :: "string \<Rightarrow> (policy * string) list"
  where "parse_input l = map (parse_line) (split CHR ''\<newline>'' (trim l))"

subsection "Solution Algorithm"

text "recursively check if a policy matches, character by character"

fun matches_policy :: "policy \<Rightarrow> string \<Rightarrow> bool"
  where "matches_policy (min_c, max_c, c) '''' = (min_c = 0)"
  | "matches_policy (min_c, max_c, c) (Cons n tail) = (
    if n = c then
      if max_c = 0 then False else
      (matches_policy (min_c-1, max_c-1, c) tail)
    else
      (matches_policy (min_c, max_c, c) tail)
    )"

text "The solution to part1 counts the number of entires in the list where the string matches
the policy"
fun part1 :: "string \<Rightarrow> natural"
  where "part1 a = (let policies = (parse_input a) in
    sum_list (map count_bool (map (\<lambda>((min_c, max_c, c),str). matches_policy (min_c, max_c, c) str) policies))
  )"

definition xor :: "bool \<Rightarrow> bool \<Rightarrow> bool" (infixl "\<oplus>" 60) 
  where "xor a b \<equiv> (a \<and> \<not>b) \<or> (b \<and> \<not>a)"

fun matches_policy_2 :: "policy \<Rightarrow> string \<Rightarrow> bool"
  where "matches_policy_2 (ca, cb, c) str = ((nth (ca-1) str) = c) \<oplus> ((nth (cb-1) str) = c)"

text "Part 2 is like part 1 but the policy is different"

fun part2 :: "string \<Rightarrow> natural"
  where "part2 a = (let policies = (parse_input a) in
    sum_list (map count_bool (map (\<lambda>((min_c, max_c, c),str). matches_policy_2 (min_c, max_c, c) str) policies))
  )"

subsection "Testing"

text "We expect our test case to return 2"

definition example_input :: "string" where "example_input = ''1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
''"

value "part1 example_input"

text "And for part 2 it should return 1"

value "part2 example_input"

export_code "part1" "part2" in Haskell module_name Solution

end
