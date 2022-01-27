section "List utility functions"

theory list_utils
  imports Main
begin

text "I can hardly believe that reduce is not built in, perhaps it is and I just can't find it"

primrec reduce :: "('a \<Rightarrow> 'b \<Rightarrow> 'a) \<Rightarrow> 'a \<Rightarrow> 'b list \<Rightarrow> 'a"
  where "reduce _ a [] = a"
  |"reduce f a (Cons b rest) = (reduce f (f a b) rest)"

end