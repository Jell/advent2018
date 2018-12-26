module Day11 (part1, part2) where
import Data.List (sortBy)
import Data.Ord (comparing)

input = 7689
gridSize = 300

power :: Int -> Int -> Int -> Int
power serial x y = (((((rackID * y) + serial) * rackID) `mod` 1000) `div` 100) - 5
                    where rackID = x + 10

generateGrid :: Int -> Int -> Int -> [[Int]]
generateGrid serial length height = [[ power serial x y | x <- [0 .. length]] | y <- [0 .. length]]

powerSquares :: Int -> [[Int]] -> [((Int, Int), Int)]
powerSquares size grid = do
  x <- [1 .. (length $ head grid) - size]
  y <- [1 .. (length $ head grid) - size]
  return ((x, y), sum [grid !! (y + dy) !! (x + dx) | dx <- [0 .. (size - 1)], dy <- [0 .. (size - 1)] ])

bestSquare :: Int -> [[Int]] -> ((Int, Int), Int)
bestSquare size grid = last $ sortBy (comparing snd) $ powerSquares size $ grid

grid :: [[Int]]
grid = generateGrid input gridSize gridSize

solve1 :: (Int, Int)
solve1 = fst $ bestSquare 3 $ grid

part1 :: IO ()
part1 = do
  let (x, y) = solve1
  putStrLn ("Day 11 - Part 1: " <> (show x) <> "," <> (show y))

solve2 :: (Int, Int, Int)
solve2 = let (s, ((x, y), _)) = last $ sortBy (comparing (snd . snd))[ (s, bestSquare s grid) | s <- [1 .. gridSize] ]
         in (x, y, s)

part2 :: IO ()
part2 = do
  let (x, y, s) = solve2
  putStrLn ("Day 11 - Part 2: " <> (show x) <> "," <> (show y) <> "," <> (show s))
