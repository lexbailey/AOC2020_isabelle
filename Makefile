
all_days=day1

default: test_all


export/AOC2020.%/code/export1/Solution.hs: %.thy
	isabelle export -d . -x'*:**' AOC2020
	sed -i -e 's/module Solution(\(.*\)Natural\(.*\)) where {/module Solution(\1 Natural(Nat) \2) where {/' $@
	sed -i -e 's/module Solution(\(.*\)Char\(.*\)) where {/module Solution(\1 Char(Char) \2) where {/' $@

.PRECIOUS:: export/AOC2020.%/code/export1/Solution.hs

objs/runner_%.o: runner.hs export/AOC2020.%/code/export1/Solution.hs
	mkdir -p objs
	ghc -c $< -o $@ -i$(dir $(word 2,$^)) -ohi objs/runner_$*.hi -cpp '-DTODAYS_INPUT=("inputs/$*.txt")' "-D$$(grep '^part2 ::' $(word 2,$^) > /dev/null && echo 'RUN_PART2' || echo 'SKIP_PART2')"
    
objs/%.o: export/AOC2020.%/code/export1/Solution.hs
	mkdir -p objs
	ghc -c $< -o $@

bin/%: objs/%.o objs/runner_%.o
	mkdir -p bin
	ghc $^ -o $@

.PRECIOUS:: objs/%.o objs/runner_%.o

test_all: $(patsubst %,bin/%,$(all_days))

.PHONY: default test_all

.SUFFIXES:

clean:
	rm -rf objs bin export output
