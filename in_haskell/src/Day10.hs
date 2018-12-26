module Day10 (part1and2) where
import Control.Category ((>>>))
import Data.Set (Set, fromList, member)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec

loadInput :: IO (String)
loadInput = readFile "../inputs/day10.txt"

integer :: GenParser Char str Int
integer = read <$> many (digit <|> char '-')

type Position = (Int, Int)
type Speed = (Int, Int)
type Dot = (Position, Speed)

dotParser :: GenParser Char str Dot
dotParser = do
  string "position=<"
  many (char ' ')
  x <- integer
  char ','
  many (char ' ')
  y <- integer
  string "> velocity=<"
  many (char ' ')
  vx <- integer
  char ','
  many (char ' ')
  vy <- integer
  string ">\n"
  return ((x, y), (vx, vy))

parseDots :: String -> [Dot]
parseDots = (either (const []) id <$> parse (many dotParser) "")


area :: [Dot] -> Int
area dots = ((maximum xs) - (minimum xs)) * ((maximum ys) - (minimum ys))
    where xs = map (fst . fst) dots
          ys = map (snd . fst) dots

updateDot :: Dot -> Dot
updateDot ((x, y), (vx, vy)) = ((x + vx, y + vy), (vx, vy))


converge :: Int -> [Dot] -> (Int, [Dot])
converge timer dots = let before = area dots
                          new = map updateDot dots
                          after = area new
                in if before > after
                   then converge (timer + 1) new
                   else (timer, dots)

render :: [Dot] -> String
render dots = intercalate "\n"
               [intercalate ""
                 [if (member (x, y) locations) then "#" else " "
                      | x <- [(minimum xs) .. (maximum xs)]
                 ] | y <- [(minimum ys) .. (maximum ys)]]
    where locations = fromList $ map fst dots
          xs = map (fst . fst) dots
          ys = map (snd . fst) dots

part1and2 :: IO ()
part1and2 = do
  dots <- parseDots <$> loadInput
  let converged = converge 0 dots
  putStrLn "Day 10 - Part 1: "
  putStrLn $ render $ snd $ converged
  putStr "Day 10 - Part 2: "
  putStrLn $ show $ fst $ converged
