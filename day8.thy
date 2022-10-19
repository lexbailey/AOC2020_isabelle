section "Solution to Day 8 of AoC 2020"

theory day8
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils list_utils HOL.Option
begin

text "This is a solution to the puzzle for day 8"

subsection "Input parsing"

datatype instr =
  Nop integer
  |Acc integer
  |Jmp integer

type_synonym program = "instr list"

definition parse_instr
  where "parse_instr s = (
    case hd s of
      CHR ''n'' \<Rightarrow> Nop (str_to_int (drop 4 s))
      |CHR ''j'' \<Rightarrow> Jmp (str_to_int (drop 4 s))
      |CHR ''a'' \<Rightarrow> Acc (str_to_int (drop 4 s))
  )"

lemma "parse_instr ''acc +7'' = Acc 7"
  by eval

lemma "parse_instr ''jmp -4'' = Jmp (- 4)"
  by eval

definition parse_program :: "string \<Rightarrow> program"
  where "parse_program p = map parse_instr (split CHR ''\<newline>'' (trim p))"

subsection "Solution Algorithm"

definition exec_instr :: "instr \<Rightarrow> integer \<Rightarrow> integer \<times> integer"
  where "exec_instr instr acc = (
    case instr of
      Nop n \<Rightarrow> (acc, 1)
      | Acc n \<Rightarrow> (acc + n, 1)
      | Jmp n \<Rightarrow> (acc, n)
  )"

definition replace
  where "replace n l new = (take_natural n l) @ [new] @ (drop (n + 1) l)"

fun exec_program_until_loop :: "natural \<Rightarrow> _"
  where
    "exec_program_until_loop limit p flags acc pc = (
    if limit = 0 then undefined else (
      if nth pc flags then
        acc
      else
        (let (new_acc, pc_change) = (exec_instr (nth pc p) acc) in
          let new_flags = replace pc flags True in
          exec_program_until_loop (limit - 1) p new_flags new_acc (natural_of_integer ((integer_of_natural pc) + pc_change))
        )
      )
    )
  "

definition find_loop
  where "find_loop p = (let flags = map (\<lambda> a . False) p in
    exec_program_until_loop 9999 p flags 0 0
  )"

definition part1 :: "string \<Rightarrow> integer"
  where "part1 s = find_loop (parse_program (trim s))"

fun fix_error
  where "fix_error (Nop n) = (Jmp n)"
  |"fix_error (Jmp n) = (Nop n)"
  |"fix_error other = other"


fun exec_program_to_exit :: "natural \<Rightarrow> _"
  where
    "exec_program_to_exit limit error_pc proglen p flags acc pc = (
    if limit = 0 then undefined else (
      if pc \<ge> proglen then
        ((Some error_pc), acc)
      else
        if nth pc flags then
          (None, acc)
        else
          (let (new_acc, pc_change) = (exec_instr (if (error_pc = pc) then (fix_error (nth pc p)) else (nth pc p)) acc) in
            let new_flags = replace pc flags True in
            exec_program_to_exit (limit - 1) error_pc proglen p new_flags new_acc (natural_of_integer ((integer_of_natural pc) + pc_change))
          )
        )
      
    )
  "

fun first_some :: "'a list \<Rightarrow> ('a \<Rightarrow> 'b option) \<Rightarrow> 'a option" where
  "first_some [] f = None"
  |"first_some (Cons h t) f = (if Option.is_none (f h) then first_some t f else Some h)"

definition part2 :: "string \<Rightarrow> integer"
  where "part2 s = (let program = parse_program (trim s) in
    (let proglen = natural_len program in
      (let flags = map (\<lambda> a . False) program in
        (let result = first_some (map (\<lambda> n . exec_program_to_exit 9999 n proglen program flags 0 0) (naturals_to proglen)) (fst) in
          (case (map_option (snd) result) of
            None \<Rightarrow> 0
            |Some (acc) \<Rightarrow> acc
          )
        )
      )
    )
  )"

subsection "Testing"

definition example_input::string where "example_input = ''
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
''"

lemma "part1 example_input = 5"
  by eval

lemma "part2 example_input = 8"
  by eval

export_code "part1" "part2" in Haskell module_name Solution

end
