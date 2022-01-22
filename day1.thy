theory day1
  imports Main "HOL.Code_Numeral"
begin

section "Solution to Day 1 of AoC 2020"

text "This is a solution to the puzzle for day 1"

text "For converting numbers in strings into naturals, we have a table of character values to
naturals"

fun digit :: "char \<Rightarrow> natural"
  where "digit CHR ''0'' = 0"
  |"digit CHR ''1'' = 1"
  |"digit CHR ''2'' = 2"
  |"digit CHR ''3'' = 3"
  |"digit CHR ''4'' = 4"
  |"digit CHR ''5'' = 5"
  |"digit CHR ''6'' = 6"
  |"digit CHR ''7'' = 7"
  |"digit CHR ''8'' = 8"
  |"digit CHR ''9'' = 9"
  |"digit a = 0"

text "parse\\_rest recursively parses the input text, if another digit appears then it multiplies
the current stored value by 10 and adds the new digit, newlines cause the current stored value to be
added to the list"

fun parse_rest :: "string \<Rightarrow> natural \<Rightarrow> natural list"
  where "(parse_rest Nil n) = Nil"
  |"(parse_rest (Cons (CHR ''\<newline>'') cs) n) = (Cons n (parse_rest cs 0))"
  |"(parse_rest (Cons c cs) n) = parse_rest cs ((n * 10) + (digit c))"

text "parse\\_rest is wrapped in a function called parse\\_input that passes in the start state for
the stored value"

fun parse_input :: "string \<Rightarrow> natural list"
  where "parse_input s = (parse_rest s 0)"

text "prod\\_of\\_sum takes a target number and a list of input numbers, finds the first pair that
sums to the target number, and returns the product of those two numbers"

fun prod_of_sum :: "natural \<Rightarrow> natural list \<Rightarrow> natural"
  where "prod_of_sum t (Cons h rest) = (if (List.member rest (t - h)) then (h * (t - h)) else (prod_of_sum t rest))"
  |"prod_of_sum t Nil = 0"

text "The solution to part 1 is simply (@{const prod_of_sum} 2020) applied to the input list"

fun part1 :: "String.string \<Rightarrow> natural"
  where "part1 a = (prod_of_sum 2020 (parse_input a))"

fun product :: "natural * natural \<Rightarrow> natural"
  where "product (a, b) = a * b"


fun prod3_of_sum :: "natural \<Rightarrow> natural list \<Rightarrow> natural"
  where "prod3_of_sum t (Cons h rest) =
    (let p = (h * (prod_of_sum (t - h) rest)) in
        (if (p = 0) then (prod3_of_sum t rest) else p)
    ) 
   "
  |"prod3_of_sum t Nil = 0"

fun part2 :: "String.string \<Rightarrow> natural"
  where "part2 s = prod3_of_sum 2020 (parse_input s)"

export_code "part1" "part2" in Haskell module_name Solution

text "We expect our test case to return 514579"

definition example_input :: "string" where "example_input = ''1721
979
366
299
675
1456
''"

value "part1 example_input"

text "We expect our test case for part 2 to return 241861950"

value "part2 example_input"

end
