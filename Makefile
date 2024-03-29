
all_days=day1 day2 day3 day4 day5 day6 day7 day8

default: test_all

export/AOC2020.%/code/export1/Solution.hs: %.thy
	isabelle export -d . -x'*:**' AOC2020
	sed -i -e 's/module Solution(\(.*\)Natural\(.*\)) where {/module Solution(\1 Natural(Nat) \2) where {/' export/AOC2020.*/code/export1/Solution.hs
	sed -i -e 's/module Solution(\(.*\)Char\(.*\)) where {/module Solution(\1 Char(Char) \2) where {/' export/AOC2020.*/code/export1/Solution.hs
	find export/AOC2020.*/code/export1/ -name Solution.hs -exec bash -c 'grep "module Solution.*Natural" {} > /dev/null || sed -i -e "s/module Solution(/module Solution ( Natural(Nat),/" {}' ';'

.PRECIOUS:: export/AOC2020.%/code/export1/Solution.hs

objs/runner_%.o: runner.hs export/AOC2020.%/code/export1/Solution.hs
	mkdir -p objs
	ghc -c $< -o $@ -i$(dir $(word 2,$^)) -ohi objs/runner_$*.hi -cpp '-DDAY_NUMBER=("$*")' '-DTODAYS_INPUT=("inputs/$*.txt")' "-D$$(grep '^part2 ::' $(word 2,$^) > /dev/null && echo 'RUN_PART2' || echo 'SKIP_PART2')" "-DINTEGER_SUPPORT=0$$(grep 'part[1-2] *:: *\[Char\] *-> *Integer;' $(word 2,$^) > /dev/null && echo '1')" "-DP1INTEGER=0$$(grep 'part1 *:: *\[Char\] *-> *Integer;' $(word 2,$^) > /dev/null && echo '1')" "-DP2INTEGER=0$$(grep 'part2 *:: *\[Char\] *-> *Integer;' $(word 2,$^) > /dev/null && echo '1')"

    
objs/%.o: export/AOC2020.%/code/export1/Solution.hs
	mkdir -p objs
	ghc -c $< -o $@

bin/%: objs/%.o objs/runner_%.o
	mkdir -p bin
	ghc $^ -o $@

.PRECIOUS:: objs/%.o objs/runner_%.o

empty:=
space:=$(space) $(space)

run_all: $(patsubst %,bin/%,$(all_days))
	$(subst $(space),;,$^)

test_all: $(patsubst %,bin/%,$(all_days))
	for day in $(all_days); do ./test $$day; done

documented_thy_files=$(patsubst %,%.thy,$(all_days)) list_natural_utils.thy natural_utils.thy string_utils.thy

output/document.pdf: ROOT document/root.tex $(documented_thy_files)
	isabelle document -d . -P output AOC2020

.PHONY: test_all

.SUFFIXES:

clean:
	rm -rf objs bin export output
