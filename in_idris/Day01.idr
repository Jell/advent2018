module Day01
import Prelude.Stream
import Data.SortedSet
%default total

parseInput : String -> List Int
parseInput = map cast . lines

solve1 : List Int -> Int
solve1 = foldl (+) 0

partial
subsolve2 : List Int -> List Int -> SortedSet Int -> Int -> Int
subsolve2 origin [] seen freq = subsolve2 origin origin seen freq
subsolve2 origin (i :: is) seen freq =
  let freq' = i + freq
  in if contains freq' seen
     then freq'
     else subsolve2 origin is (insert freq' seen) freq'

partial
solve2 : List Int -> Int
solve2 input = subsolve2 input empty empty 0

export
part1 : IO ()
part1 = do
  Right text <- readFile "../inputs/day01.txt"
        | Left f => printLn f
  putStr "Day 01 - Part 1: "
  printLn $ solve1 $ parseInput text

export partial
part2 : IO ()
part2 = do
  Right text <- readFile "../inputs/day01.txt"
        | Left f => printLn f
  putStr "Day 01 - Part 2: "
  printLn $ solve2 $ parseInput text
