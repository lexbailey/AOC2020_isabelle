section "Solution to Day 6 of AoC 2020"

theory day6
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils list_utils
begin

text "This is a solution to the puzzle for day 6"

subsection "Input parsing"

text "We want a list of lists of sets of chars representing the list of groups which are lists of
people who responded yes to each question in a set of questions, and no to any other question.
Except that sets are awkward (as a datatype in isabelle) so we use lists instead, with our own
functions for insert (set\\_cons) and a set-from-list function (uniq)"

fun set_cons :: "'a \<Rightarrow> 'a list \<Rightarrow> 'a list"
  where "set_cons a l = (if contains a l then l else Cons a l)"

fun uniq :: "'a list \<Rightarrow> 'a list"
  where "uniq [] = []"
  | "uniq (Cons h t) = (set_cons h (uniq t))"

definition parse_input :: "string \<Rightarrow> char list list list"
  where "parse_input a = (
    map (map uniq \<circ> split CHR '' '')
    (split CHR ''\<newline>'' 
      (join_lines CHR '' '' (trim a))
    )
  )"

subsection "Solution Algorithm"

text "In part 1 we just sum the sizes of the sets for each group of people"

definition union :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list"
  where "union a b = uniq (a @ b)"

definition part1 :: "string \<Rightarrow> natural"
  where "part1 s = (let data = parse_input s in
    sum_list (
    map (
      natural_len \<circ> (reduce union [])
    ) data)
  )"

text "For part 2 we just use the intersection of all the sets, instead of the union"

definition intersection :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list"
  where "intersection a b = (filter (\<lambda>x. contains x b) a)"

fun intersect_all :: "'a list list \<Rightarrow> 'a list"
  where "intersect_all [] = []"
  | "intersect_all (Cons h t) = reduce intersection h t"

definition part2 :: "string \<Rightarrow> natural"
  where "part2 s = (let data = parse_input s in
    sum_list (
    map (
      natural_len \<circ> intersect_all
    ) data)
  )"

subsection "Testing"

text "This example input should return 11"

definition example_input::string where "example_input = ''
abc

a
b
c

ab
ac

a
a
a
a

b
''"

value "part1 example_input"

text "The same example input for part 2 should return 6"

value "part2 example_input"

export_code "part1" "part2" in Haskell module_name Solution

end
