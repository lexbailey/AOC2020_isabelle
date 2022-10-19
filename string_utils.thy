section "String processing utilities"

theory string_utils
  imports Main
begin

subsection "Splitting"

text "split\\_once splits a string into two string on a separator"

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

text "split"

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

text "This example evaluates to [''foo'', ''bar'', ''baz'']"

value "split CHR '':'' ''foo:bar:baz''"

subsection "Triming"

text "trim\\_end for removing whitespace at the end of a string"

fun trim_end :: "string \<Rightarrow> string"
  where "trim_end Nil = Nil"
  | "trim_end (Cons c rest) = (let end = (trim_end rest) in (
      (if (c = CHR '' '' \<or> c = CHR ''\<newline>'') then
        (if end = '''' then '''' else (Cons c end))
      else
        Cons c end
      )
    ))"

text "trim removes whitespace at both ends"

fun trim :: "string \<Rightarrow> string"
  where "trim Nil = Nil"
  |"trim (Cons c rest) = (
    if (c = CHR '' '' \<or> c = CHR ''\<newline>'') then
      trim rest
    else
      Cons c (trim_end rest)
  )"

text "Both trim and trim\\_end remove spaces and newlines"
text "In this example only ''foo'' is left"

value "trim '' \<newline>\<newline>   foo   \<newline>\<newline>\<newline>  ''"

subsection "Number Parsing"

text "For converting numbers in strings into naturals, we start with a table of character values to
naturals"

fun digit :: "char \<Rightarrow> natural"
  where "digit c = (case c of
    CHR ''0'' \<Rightarrow> 0
    | CHR ''1'' \<Rightarrow> 1
    | CHR ''2'' \<Rightarrow> 2
    | CHR ''3'' \<Rightarrow> 3
    | CHR ''4'' \<Rightarrow> 4
    | CHR ''5'' \<Rightarrow> 5
    | CHR ''6'' \<Rightarrow> 6
    | CHR ''7'' \<Rightarrow> 7
    | CHR ''8'' \<Rightarrow> 8
    | CHR ''9'' \<Rightarrow> 9
  )"

text "Next we need to be able to parse naturals"

fun str_to_nat_base_partial :: "natural \<Rightarrow> natural \<Rightarrow> string \<Rightarrow> natural"
  where "str_to_nat_base_partial b x Nil = x"
  | "str_to_nat_base_partial b x (Cons y rest) = str_to_nat_base_partial b ((x * b) + (digit y)) rest"
(*
fun str_to_nat_partial :: "natural \<Rightarrow> string \<Rightarrow> natural"
  where "str_to_nat_partial x Nil = x"
  | "str_to_nat_partial x (Cons y rest) = str_to_nat_partial ((x * 10) + (digit y)) rest"
*)
definition str_to_nat_base :: "natural \<Rightarrow> string \<Rightarrow> natural"
  where "str_to_nat_base b = str_to_nat_base_partial b 0"

definition str_to_nat :: "string \<Rightarrow> natural"
  where "str_to_nat = str_to_nat_base_partial 10 0"

fun str_to_int :: "string \<Rightarrow> integer"
  where "str_to_int s = (case hd s of
    CHR ''+'' \<Rightarrow>integer_of_natural (str_to_nat (tl s))
    |CHR ''-'' \<Rightarrow> - integer_of_natural (str_to_nat (tl s))
    | c \<Rightarrow> integer_of_natural (str_to_nat s)
  )"

text "Some tests for classes of characters"

definition is_digit :: "char \<Rightarrow> bool"
  where "is_digit a = (case a of
    (CHR ''0'') \<Rightarrow> True
    |(CHR ''1'') \<Rightarrow> True
    |(CHR ''2'') \<Rightarrow> True
    |(CHR ''3'') \<Rightarrow> True
    |(CHR ''4'') \<Rightarrow> True
    |(CHR ''5'') \<Rightarrow> True
    |(CHR ''6'') \<Rightarrow> True
    |(CHR ''7'') \<Rightarrow> True
    |(CHR ''8'') \<Rightarrow> True
    |(CHR ''9'') \<Rightarrow> True
    |a \<Rightarrow> False
  )"

definition is_hexit :: "char \<Rightarrow> bool"
  where "is_hexit a = ((is_digit a) \<or> (case a of
    (CHR ''a'') \<Rightarrow> True
    |(CHR ''b'') \<Rightarrow> True
    |(CHR ''c'') \<Rightarrow> True
    |(CHR ''d'') \<Rightarrow> True
    |(CHR ''e'') \<Rightarrow> True
    |(CHR ''f'') \<Rightarrow> True
    |a \<Rightarrow> False
  ))"

fun join_lines :: "char \<Rightarrow> string \<Rightarrow> string"
  where "join_lines j [] = []"
  |"join_lines j [c] = [c]"
  |"join_lines j (Cons CHR ''\<newline>'' (Cons c rest)) = (
    case c of
      (CHR ''\<newline>'') \<Rightarrow> Cons (CHR ''\<newline>'') (join_lines j rest)
      | other \<Rightarrow> Cons j (join_lines j (Cons other rest))
  )"
  |"join_lines j (Cons c rest) = Cons c (join_lines j rest)"

end