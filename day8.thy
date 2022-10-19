section "Solution to Day 8 of AoC 2020"

theory day8
  imports Main "HOL.Code_Numeral" string_utils list_natural_utils natural_utils list_utils
begin

text "This is a solution to the puzzle for day 8"

subsection "Input parsing"

datatype instr =
  Nop
  |Acc integer
  |Jmp integer

type_synonym program = "instr list"

definition parse_instr
  where "parse_instr s = (
    case hd s of
      CHR ''n'' \<Rightarrow> Nop
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
      Nop \<Rightarrow> (acc, 1)
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

definition part2 :: "string \<Rightarrow> natural"
  where "part2 s = 0"

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

lemma "part2 example_input = 0"
  by eval

(* TODO remember to export part2 once complete *)

export_code "part1" in Haskell module_name Solution

end
