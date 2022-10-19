-- Solution runner, calls part1 and part2 functions with the contents of the input file
import Data.Bits
import Data.Char
import Solution;

todays_input = TODAYS_INPUT
day_num = DAY_NUMBER

input = readFile todays_input

solve part t = a
    where Nat a = part t

#if INTEGER_SUPPORT
solve_int part t = part t
#endif

unconvert_char :: Solution.Char -> Prelude.Char
unconvert_char (Char a b c d e f g h) = chr o
    where o = ((if a then 0x01 else 0)
              +(if b then 0x02 else 0)
              +(if c then 0x04 else 0)
              +(if d then 0x08 else 0)
              +(if e then 0x10 else 0)
              +(if f then 0x20 else 0)
              +(if g then 0x40 else 0)
              +(if h then 0x80 else 0))

--instance Show Solution.Char where
--    show c = show (unconvert_char c)

convert_char :: Prelude.Char -> Solution.Char
convert_char c = (Char
    ((o .&. 0x01) > 0)
    ((o .&. 0x02) > 0)
    ((o .&. 0x04) > 0)
    ((o .&. 0x08) > 0)
    ((o .&. 0x10) > 0)
    ((o .&. 0x20) > 0)
    ((o .&. 0x40) > 0)
    ((o .&. 0x80) > 0)
    )
    where o = ord c

convert_string :: [Prelude.Char] -> [Solution.Char]
convert_string s = map convert_char s

main :: IO()
main = do
    putStrLn ("Running solution for " ++ day_num ++ " (input file: " ++ todays_input ++ ")")
    text <- input
    let a = convert_string text in do
        putStrLn "Part1:"
#if P1INTEGER
        print $ solve_int part1 a
#else
        print $ solve part1 a
#endif
#ifdef RUN_PART2
        putStrLn "Part2:"
#if P2INTEGER
        print $ solve_int part2 a
#else
        print $ solve part2 a
#endif
#else
#if SKIP_PART2
        putStrLn "Part2 is skipped (no part2 function found)"
#else
#error Must define onw of RUN_PART2 or SKIP_PART2
#endif
#endif

