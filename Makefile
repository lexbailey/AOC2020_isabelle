
all_days=day1

default: test_all


export/AOC2020.%/code/export1/Solution.hs: %.thy
	isabelle export -d . -x'*:**' AOC2020
	sed -i -e 's/module Solution(\(.*\)Natural\(.*\)) where {/module Solution(\1 Natural(Nat) \2) where {/' $@
	sed -i -e 's/module Solution(\(.*\)Char\(.*\)) where {/module Solution(\1 Char(Char) \2) where {/' $@

.PRECIOUS:: export/AOC2020.%/code/export1/Solution.hs

runner_%.o: runner.hs export/AOC2020.%/code/export1/Solution.hs
	ghc -c $< -o $@ -i$(dir $(word 2,$^)) -cpp '-DTODAYS_INPUT=("inputs/$*.txt")' "-D$$(grep '^part2 ::' $(word 2,$^) > /dev/null && echo 'RUN_PART2' || echo 'SKIP_PART2')"
    
%.o: export/AOC2020.%/code/export1/Solution.hs
	ghc -c $< -o $@

%: %.o runner_%.o
	ghc $^ -o $@

.PRECIOUS:: %.o

test_all: exported_code $(all_days)

.PHONY: default test_all exported_code

.SUFFIXES:
