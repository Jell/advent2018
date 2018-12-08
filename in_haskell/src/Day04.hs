module Day04 (part1, part2) where
import Data.List (sort, group, maximumBy)
import Data.Map (Map, toList, fromListWith, empty, (!), map, filter)
import Data.Ord (comparing)
import Text.ParserCombinators.Parsec
import Control.Category ((>>>))

loadInput :: IO (String)
loadInput = unlines <$> sort <$> lines <$> readFile "../inputs/day04.txt"

parseMinute :: GenParser Char str Int
parseMinute = do
  char '['
  many digit
  char '-'
  many digit
  char '-'
  many digit
  char ' '
  many digit
  char ':'
  minute <- many alphaNum
  char ']'
  pure $ read minute

parseGuard :: GenParser Char str Int
parseGuard = do
  parseMinute
  string " Guard #"
  guardId <- many digit
  string " begins shift"
  char '\n'
  pure $ read guardId

parseNap :: GenParser Char str [Int]
parseNap = do
  start <- parseMinute
  string " falls asleep"
  char '\n'
  end <- parseMinute
  string " wakes up"
  char '\n'
  pure [start .. (end - 1)]

parseShift :: GenParser Char str (Int, [Int])
parseShift = do
    id <- parseGuard
    naps <- many $ try parseNap
    pure $ (id, concat naps)

parseShifts :: GenParser Char str (Map Int [Int])
parseShifts = fromListWith (++) <$> many parseShift

loadShifts :: IO (Map Int [Int])
loadShifts = either (const empty) id <$> parse parseShifts "" <$> loadInput

part1 :: IO ()
part1 = do
  shifts <- loadShifts
  let (longestID, longestTime) = maximumBy (comparing $ length . snd) $ toList $ shifts
  let minutes = shifts ! longestID
  let bestMinute = head $ maximumBy (comparing length) $ group $ sort $ minutes
  putStr "Day 04 - Part 1: "
  putStrLn $ show $ longestID * bestMinute

solve2 :: (Map Int [Int]) -> (Int, [Int])
solve2 = Data.Map.filter (not . null)
     >>> Data.Map.map (maximumBy (comparing length) . group . sort)
     >>> toList
     >>> maximumBy (comparing $ length . snd)

part2 :: IO ()
part2 = do
  shifts <- loadShifts
  let result = solve2 shifts
  let bestId = fst result
  let bestMinute = head $ snd result
  putStr "Day 04 - Part 2: "
  putStrLn $ show $ bestId * bestMinute
