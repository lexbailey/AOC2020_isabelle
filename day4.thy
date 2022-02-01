section "Solution to Day 4 of AoC 2020"

theory day4
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils list_utils
begin

text "This is a solution to the puzzle for day 4"

subsection "Input parsing"

text "The input for this puzzle has records separated by two newlines, where each record is split
across multiple lines. We parse it by joining together the multiline records into single lines
and collapsing the double newlines into single newlines, then splitting on newlines and parsing each
line as a record"

definition join_records :: "string \<Rightarrow> string"
  where "join_records = join_lines CHR '' ''"

text "Passport information is stored as a record of string options, since some fields can be missing"

record passport =
  byr :: "string option"
  iyr :: "string option"
  eyr :: "string option"
  hgt :: "string option"
  hcl :: "string option"
  ecl :: "string option"
  pid :: "string option"
  cid :: "string option"

definition Blank_Passport :: "passport"
  where "Blank_Passport = \<lparr>
    byr = None
    ,iyr = None
    ,eyr = None
    ,hgt = None
    ,hcl = None
    ,ecl = None
    ,pid = None
    ,cid = None
  \<rparr>"

fun update_passport :: "passport \<Rightarrow> string \<Rightarrow> passport"
  where "update_passport p s = (let (field, value) = (split_once CHR '':'' s) in
    case field of
      ''byr'' \<Rightarrow> p\<lparr> byr:=Some value \<rparr>
      |''iyr'' \<Rightarrow> p\<lparr> iyr:=Some value \<rparr>
      |''eyr'' \<Rightarrow> p\<lparr> eyr:=Some value \<rparr>
      |''hgt'' \<Rightarrow> p\<lparr> hgt:=Some value \<rparr>
      |''hcl'' \<Rightarrow> p\<lparr> hcl:=Some value \<rparr>
      |''ecl'' \<Rightarrow> p\<lparr> ecl:=Some value \<rparr>
      |''pid'' \<Rightarrow> p\<lparr> pid:=Some value \<rparr>
      |''cid'' \<Rightarrow> p\<lparr> cid:=Some value \<rparr>
  )"

text "this representation, plus the update\\_passport function which updates an arbitraty field in
a passport record, enables us to define passport parsing as simply reducing the list of fields to
parse over the update function"

definition parse_passport :: "string \<Rightarrow> passport"
  where "parse_passport r = reduce update_passport Blank_Passport (split CHR '' '' r)"

definition parse_all :: "string \<Rightarrow> passport list"
  where "parse_all s = map parse_passport (split CHR ''\<newline>'' (join_records (trim s)))"

subsection "Solution Algorithm"

text "Passports must have all fields present, or have only cid missing, the task for part1 is simply
to count the valid ones according to those rules"

definition is_some :: "'a option \<Rightarrow> bool"
  where "is_some a = (a \<noteq> None)"

definition is_valid :: "passport \<Rightarrow> bool"
  where "is_valid p = (
    is_some (byr p)
    \<and> is_some (iyr p)
    \<and> is_some (eyr p)
    \<and> is_some (hgt p)
    \<and> is_some (hcl p)
    \<and> is_some (ecl p)
    \<and> is_some (pid p)
  )"

definition num_valid :: "(passport \<Rightarrow> bool) \<Rightarrow> string \<Rightarrow> natural"
  where "num_valid f = sum_list \<circ> (map count_bool) \<circ> (map f) \<circ> parse_all"

definition part1 :: "string \<Rightarrow> natural"
  where "part1 = num_valid is_valid"

text "In part 2 we do the same but with lots of extra rules. The rules suck and you would never get
away with implementing these rules in the real world, but I guess I'll look past that for now."

definition all :: "bool list \<Rightarrow> bool"
  where "all = reduce (\<and>) True"

definition is_number_between :: "natural \<Rightarrow> natural \<Rightarrow> string \<Rightarrow> bool"
  where "is_number_between n_min n_max num = ((all (map is_digit num)) \<and> (let n = str_to_nat num in ((n_min \<le> n) \<and> (n \<le> n_max))))"

definition valid_height :: "string \<Rightarrow> bool"
  where "valid_height s = (let l = length s in
    if l = 4 then is_number_between 59 76 (fst (split_once CHR ''i'' s)) else
    if l = 5 then is_number_between 150 193 (fst (split_once CHR ''c'' s)) else
    False
  )"

fun valid_color :: "string \<Rightarrow> bool"
  where "valid_color (Cons CHR ''#'' digits) = (length digits = 6 \<and> (all (map is_hexit digits)))"
  | "valid_color s = False"

definition valid_eye_color :: "string \<Rightarrow> bool"
  where "valid_eye_color s = (
    s = ''amb''
    \<or> s = ''blu''
    \<or> s = ''brn''
    \<or> s = ''gry''
    \<or> s = ''grn''
    \<or> s = ''hzl''
    \<or> s = ''oth''
  )"

definition valid_pid :: "string \<Rightarrow> bool"
  where "valid_pid a = ((length a = 9) \<and> (all (map is_digit a)))"

fun present_and_matches :: "('a \<Rightarrow> bool) \<Rightarrow> 'a option \<Rightarrow> bool"
  where "present_and_matches _ None = False"
  | "present_and_matches f (Some a) = f a"

definition is_valid_2 :: "passport \<Rightarrow> bool"
  where "is_valid_2 p = (
    present_and_matches (is_number_between 1920 2002) (byr p)
    \<and> present_and_matches (is_number_between 2010 2020) (iyr p)
    \<and> present_and_matches (is_number_between 2020 2030) (eyr p)
    \<and> present_and_matches valid_height (hgt p)
    \<and> present_and_matches valid_color (hcl p)
    \<and> present_and_matches valid_eye_color (ecl p)
    \<and> present_and_matches valid_pid (pid p)
  )"

definition part2 :: "string \<Rightarrow> natural"
  where "part2 = num_valid is_valid_2"

subsection "Testing"

text "We expect our test case to return 2"

definition example_input::string where "example_input = ''
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
''"

value "part1 example_input"

text "For part 2 the example also returns 2"

value "part2 example_input"

export_code "part1" "part2" in Haskell module_name Solution

end
