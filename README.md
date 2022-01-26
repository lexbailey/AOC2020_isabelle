# My solutions to AoC 2020 using the Isabelle interactive theorem prover

I'm using it just as a functional programming language really

Code for each day is exported to haskell and executed via a small haskell wrapper that just loads the input file and calls a function exported from isabelle

## Running

make the solution for a particular day like so:

    make bin/day1

then you can run it

    ./bin/day1

you can also build and run all of the solutions at once

    make test_all

And just for fun there's a rule to build a PDF document

    make output/document.pdf
