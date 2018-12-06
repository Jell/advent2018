module Day01 (part1, part2) where
import qualified Data.Set

parseInt :: String -> Int
parseInt x = read $ filter (`elem` "-0123456789") x

loadInput :: IO ([Int])
loadInput = do
  raw <- readFile "../inputs/day01.txt"
  return $ map parseInt $ lines raw

part1 :: IO ()
part1 = do
  input <- loadInput
  putStr "Day 01 - Part 1: "
  putStrLn $ show $ foldl (+) 0 input

subsolve2 :: [Int] -> [Int] -> Data.Set.Set Int -> Int -> Int
subsolve2 origin [] seen freq = subsolve2 origin origin seen freq
subsolve2 origin (i : is) seen freq =
  let freq' = i + freq
  in if Data.Set.member freq' seen
     then freq'
     else subsolve2 origin is (Data.Set.insert freq' seen) freq'

solve2 :: [Int] -> Int
solve2 input = subsolve2 input [] Data.Set.empty 0

part2 :: IO ()
part2 = do
  input <- loadInput
  putStr "Day 01 - Part 2: "
  putStrLn $ show $ solve2 input
