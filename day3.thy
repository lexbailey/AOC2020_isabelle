section "Solution to Day 3 of AoC 2020"

theory day3
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils
begin

text "This is a solution to the puzzle for day 3"

subsection "Input parsing"

definition is_tree :: "char \<Rightarrow> bool"
  where "is_tree c = (c = CHR ''#'')"

definition parse_input :: "string \<Rightarrow> bool list list"
  where "parse_input a = map (map is_tree) (split CHR ''\<newline>'' (trim a))"

subsection "Solution Algorithm"

fun trees_hit :: "bool list list \<Rightarrow> natural \<Rightarrow> natural \<Rightarrow> natural"
  where"trees_hit [] _ _ = 0"
  |"trees_hit (Cons h t) \<Delta>x x = ((count_bool ((nth_mod_len x h))) + (trees_hit t \<Delta>x (x+\<Delta>x)))"

definition scan_trajectory ::"bool list list \<Rightarrow> natural \<Rightarrow> natural \<Rightarrow> natural"
  where "scan_trajectory grid x y = (trees_hit (skip_each (y-1) grid) x 0)"

text "The solution to part1 counts the number of trees we will hit on a 3:1 slope"

fun part1 :: "string \<Rightarrow> natural"
  where "part1 a = (scan_trajectory (parse_input a) 3 1)"

text "In part 2 we need the product of a few different trajectory totals"

fun part2 :: "string \<Rightarrow> natural"
  where "part2 a = (let scan_grid = scan_trajectory (parse_input a) in prod_list (map2 scan_grid
    [1, 3, 5, 7, 1]
    [1, 1, 1, 1, 2]
  ))"

subsection "Testing"

text "We expect our test case to return 7"

definition example_input::string where "example_input = ''
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
''"

value "part1 example_input"

text "For part 2 the example should return 336"

value "part2 example_input"

export_code "part1" "part2" in Haskell module_name Solution

end
