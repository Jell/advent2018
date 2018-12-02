module Main
import Data.Vect
import Day01 as Day01
import Day02 as Day02

main : IO ()
main = do Day01.part1
          Day01.part2

          putStrLn "Day 2"
          printLn Day02.part1
          printLn Day02.part2
