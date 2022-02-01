section "List Utilities using the HOL.Code\\_Numeral natural type"

theory list_natural_utils
  imports Main "HOL.Code_Numeral"
begin

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

fun upto_natural :: "natural \<Rightarrow> natural \<Rightarrow> natural list" ("(1[_...</_'])")
  where "upto_natural i j = (if j = 0 then [] else (if i \<le> j-1 then [i...<j-1] @ [j-1] else []))"

end