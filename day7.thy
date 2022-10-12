section "Solution to Day 7 of AoC 2020"

theory day7
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils list_utils
begin

text "This is a solution to the puzzle for day 6"

subsection "Input parsing"

primrec bag_desc :: "string \<Rightarrow> (string list) \<Rightarrow> (string * (string list))"
  where
    "bag_desc parsed [] = ('''', [''''])"
    |"bag_desc parsed (Cons t toks) = (
      if t = ''bags'' then
        (parsed, tl toks)
      else
        bag_desc (parsed @ '' '' @ t) toks
    )"

fun sub_bags_parse :: "(natural * string) list \<Rightarrow> (natural * string) \<Rightarrow> string list \<Rightarrow> (natural * string) list"
  where
    "sub_bags_parse a part [] = a"
  |"sub_bags_parse a part (Cons n rest) =
    (let (part_n, part_label) = part in
    (if contains n [''bag,'',''bags,'',''bag.'',''bags.''] then
       sub_bags_parse (a @ [(part_n, trim part_label)]) (0, '''') rest
    else
      (if part_n = 0 then
        if n = ''no'' then
          a
        else
        (let next_n = str_to_nat n in
          sub_bags_parse a (next_n, '''') rest
        )
      else
        sub_bags_parse a (part_n, part_label @ '' '' @ n) rest
      )
    )
)"

definition parse_line :: "string \<Rightarrow> (string * ((natural * string) list))"
  where "parse_line line = (let t = split CHR '' '' line in
    let (bd, sub_bags) = bag_desc '''' t in
    (trim bd, sub_bags_parse [] (0, '''') sub_bags)
  )"

definition parse_all where "parse_all s = map parse_line (split CHR ''\<newline>'' (trim s))"

subsection "Solution Algorithm"

definition p1col where "p1col = ''shiny gold''"

primrec get_bag_info where
  "get_bag_info [] bag_col = undefined"
|"get_bag_info (Cons bag1_def bag_defs) bag_col = 
  (if fst bag1_def = bag_col then snd bag1_def else get_bag_info bag_defs bag_col)
  "

definition all_bag_names where "all_bag_names bag_defs = map fst bag_defs"

definition can_hold where "can_hold bag_defs bag_col =
  (let names = all_bag_names bag_defs in
    filter (\<lambda> name . contains bag_col (map snd (get_bag_info bag_defs name))) names
  )
"

fun can_hold_rec :: "natural \<Rightarrow> (string \<times> (natural \<times> string) list) list \<Rightarrow> string list \<Rightarrow> string list" where
"can_hold_rec limit bag_defs bag_cols = (if limit = 0 then undefined else  (let new_list =
  (reduce uniq_ins bag_cols (flatten (map (can_hold bag_defs) bag_cols)))
  in
  if natural_len new_list = natural_len bag_cols then bag_cols else can_hold_rec (limit - 1) bag_defs new_list
))
"

definition part1 :: "string \<Rightarrow> natural"
  where "part1 s = (natural_len (can_hold_rec 99 (parse_all s) [p1col])) - 1"

fun num_held :: "natural \<Rightarrow> _"
  where "num_held limit bag_defs bag_col = 
  (if limit = 0 then undefined else
    (let subbags = get_bag_info bag_defs bag_col in
      (if (natural_len subbags) = 0 then 1 else
        (reduce (+) 0 (map (\<lambda> (n, name) . n * (num_held (limit - 1) bag_defs name)) subbags)) + 1
      )
    )
  )"

definition part2 :: "string \<Rightarrow> natural"
  where "part2 s = (let bags = parse_all s in
    (num_held 99 bags p1col) - 1
  )"

subsection "Testing"

definition example_input::string where "example_input = ''
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
''"

lemma "part1 example_input = 4"
  by eval

lemma "part2 example_input = 32"
  by eval

export_code "part1" "part2" in Haskell module_name Solution

end
