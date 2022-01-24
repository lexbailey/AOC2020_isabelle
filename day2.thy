section "Solution to Day 2 of AoC 2020"

theory day2
  imports Main "HOL.Code_Numeral"
begin

text "This is a solution to the puzzle for day 2"

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

text "Next we need some text processing functions"

fun split_once :: "char \<Rightarrow> string \<Rightarrow> (string * string)"
  where "split_once c (Cons n str) = (
    if c = n then (
      '''', str
    ) else (
      let (s1, s2) = split_once c str in
      ((Cons n s1), s2)
    )
  )"
  | "split_once c Nil = (Nil,Nil)"

text "example, get the first item of ''foo:bar:baz'' splitting in '':''
to get ''foo'' and ''bar:baz'' "
value "split_once (CHR '':'') ''foo:bar:baz''"

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

text "Now we can split all items, our text correctly evaluates to [''foo'', ''bar'', ''baz'']"

value "split CHR '':'' ''foo:bar:baz''"

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

text "trim is required because there might be a trailing newline in the file, which results in an
extra empty element at the end of the list once we split the input on newlines"

value "trim '' \<newline>\<newline>   foo   \<newline>\<newline>\<newline>  ''"

text "Next we need to be able to parse integers"

fun str_to_nat_partial :: "natural \<Rightarrow> string \<Rightarrow> natural"
  where "str_to_nat_partial x Nil = x"
  | "str_to_nat_partial x (Cons y rest) = str_to_nat_partial ((x * 10) + (digit y)) rest"

fun str_to_nat :: "string \<Rightarrow> natural"
  where "str_to_nat l = str_to_nat_partial 0 l"

value "str_to_nat ''543''"

text "now we are ready to parse the policies from the file"

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

definition example_input :: "string" where "example_input = ''1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
''"

value "split CHR ''\<newline>'' (trim example_input)"
value "parse_input example_input"

primrec sum :: "natural list \<Rightarrow> natural"
  where "sum [] = 0"
  | "sum (Cons n tail) = (n + sum tail)"

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

fun count_bool :: "bool \<Rightarrow> natural"
  where "count_bool True = 1"
  | "count_bool False = 0"

text "The solution to part1 counts the number of entires in the list where the string matches
the policy"
fun part1 :: "string \<Rightarrow> natural"
  where "part1 a = (let policies = (parse_input a) in
    sum (map count_bool (map (\<lambda>((min_c, max_c, c),str). matches_policy (min_c, max_c, c) str) policies))
  )"

export_code "part1" in Haskell module_name Solution

text "We expect our test case to return 2"

value "part1 example_input"

(*

value "part2 example_input"
*)
end
