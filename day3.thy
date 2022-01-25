section "Solution to Day 3 of AoC 2020"

theory day3
  imports Main "HOL.Code_Numeral"
begin

text "This is a solution to the puzzle for day 3"

subsection "String manipulation functions"

text "We need to be able to split a string in parts on a given character"

fun split :: "'a \<Rightarrow> 'a list \<Rightarrow> 'a list list"
  where "split c Nil = [[]]"
  |"split c (Cons next rest) = (
    (let sub_list = (split c rest) in
      (if c = next then (
        Cons [] sub_list
      ) else (
        (case sub_list of
          Nil \<Rightarrow> (Cons [next] Nil)
          | (Cons a rest) \<Rightarrow> (Cons (Cons next a) rest)
        )
      ))
    )
  )"

text "trim function for discarding leading and trailing whitespace"

fun trim_end :: "string \<Rightarrow> string"
  where "trim_end Nil = Nil"
  | "trim_end (Cons c rest) = (let end = (trim_end rest) in (
      (if (c = CHR '' '' \<or> c = CHR ''\<newline>'') then
        (if end = '''' then '''' else (Cons c end))
      else
        Cons c end
      )
    ))"

fun trim :: "string \<Rightarrow> string"
  where "trim Nil = Nil"
  |"trim (Cons c rest) = (
    if (c = CHR '' '' \<or> c = CHR ''\<newline>'') then
      trim rest
    else
      Cons c (trim_end rest)
  )"

subsection "Input parsing"

definition is_tree :: "char \<Rightarrow> bool"
  where "is_tree c = (c = CHR ''#'')"

definition parse_input :: "string \<Rightarrow> bool list list"
  where "parse_input a = map (map is_tree) (split CHR ''\<newline>'' (trim a))"

subsection "Some biolerplate for dealing with lists in the Code\\_Numeral natural type"

primrec natural_len :: "'a list \<Rightarrow> natural"
  where "natural_len [] = 0"
  |"natural_len (Cons h t) = 1 + natural_len t"

fun nth :: "natural \<Rightarrow> 'a list \<Rightarrow> 'a"
  where "nth n [] = undefined"
  | "nth n (Cons h t) = (if n=0 then h else nth (n-1) t)"

primrec drop:: "natural \<Rightarrow> 'a list \<Rightarrow> 'a list"
  where drop_Nil: "drop n [] = []" |
  drop_Cons: "drop n (x # xs) = (case n of 0 \<Rightarrow> x # xs | Code_Numeral.Suc m \<Rightarrow> drop m xs)"

definition natural_mod :: "natural \<Rightarrow> natural \<Rightarrow> natural" (infixl "nmod" 60)
  where "natural_mod a b = a - (b*(a div b))"

fun nth_mod_len :: "natural \<Rightarrow> 'a list \<Rightarrow> 'a"
  where "nth_mod_len n l = nth (n nmod (natural_len l)) l"

fun "naturals_to" :: "natural \<Rightarrow> natural list"
  where "naturals_to a = (case a of 0 \<Rightarrow> [0]
  | (Code_Numeral.Suc m) \<Rightarrow> ((naturals_to m) @ [(Code_Numeral.Suc m)]))"

definition enum_natural :: "'a list \<Rightarrow> (natural * 'a) list"
  where "enum_natural xs = (zip (naturals_to (natural_len xs)) xs)"

definition skip_each :: "natural \<Rightarrow> 'a list \<Rightarrow> 'a list"
  where "skip_each n l = map snd ((filter (\<lambda>(i,v). i nmod (n+1) = 0) (enum_natural l)))"

subsection "Solution code"

fun count_bool :: "bool \<Rightarrow> natural"
  where "count_bool True = 1"
  | "count_bool False = 0"

fun trees_hit :: "bool list list \<Rightarrow> natural \<Rightarrow> natural \<Rightarrow> natural"
  where"trees_hit [] _ _ = 0"
  |"trees_hit (Cons h t) \<Delta>x x = ((count_bool ((nth_mod_len x h))) + (trees_hit t \<Delta>x (x+\<Delta>x)))"

definition scan_trajectory ::"bool list list \<Rightarrow> natural \<Rightarrow> natural \<Rightarrow> natural"
  where "scan_trajectory grid x y = (trees_hit (skip_each (y-1) grid) x 0)"

text "The solution to part1 counts the number of trees we will hit on a 3:1 slope"

fun part1 :: "string \<Rightarrow> natural"
  where "part1 a = (scan_trajectory (parse_input a) 3 1)"

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

text "In part 2 we need the product of a few different trajectory totals"

fun part2 :: "string \<Rightarrow> natural"
  where "part2 a = (let scan_grid = scan_trajectory (parse_input a) in prod_list (map2 scan_grid
    [1, 3, 5, 7, 1]
    [1, 1, 1, 1, 2]
  ))"

text "For part 2 the example should return 336"

value "part2 example_input"

export_code "part1" "part2" in Haskell module_name Solution

end
