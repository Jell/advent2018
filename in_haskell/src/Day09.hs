module Day09 (part1, part2) where
import Control.Category ((>>>))
import Data.List (intercalate, foldl')
import Data.Map (Map, adjust, fromList, elems)
import Text.ParserCombinators.Parsec

type Board = ([Int], Int, [Int])
type Scores = Map Int Int

loadInput :: IO (String)
loadInput = readFile "../inputs/day09.txt"

integer :: GenParser Char str Int
integer = read <$> many digit

playersAndLimit :: GenParser Char str (Int, Int)
playersAndLimit = do
  players <- integer
  string " players; last marble is worth "
  limit <- integer
  string " points"
  return (players, limit)

parsePlayersAndLimit :: String -> (Int, Int)
parsePlayersAndLimit = (either (const (0, 0)) id <$> parse playersAndLimit "")

render :: Board -> String
render (lefts, c, rights) = (intercalate " " (map show (reverse lefts))) <> " (" <> show c <> ") " <> (intercalate " " (map show rights))

empty :: Board
empty = ([], 0, [])

moveRight :: Board -> Board
moveRight ([], c, []) = ([], c, [])
moveRight (lefts, c, []) = moveRight ([], c, reverse lefts)
moveRight (lefts, c, r : rights) = (c : lefts, r, rights)

moveLeft :: Board -> Board
moveLeft ([], c, []) = ([], c, [])
moveLeft ([], c, rights) = moveLeft (reverse rights, c, [])
moveLeft (l : lefts, c, rights) = (lefts, l, c : rights)

insertRight :: Int -> Board -> Board
insertRight i (lefts, c, rights) = (c : lefts, i, rights)

pop :: Board -> (Int, Board)
pop ([], c, []) = undefined
pop (lefts, c, []) = pop ([], c, reverse lefts)
pop (lefts, c, r : rights) = (c, (lefts, r, rights))

moveAndPop :: Board -> (Int, Board)
moveAndPop = (moveLeft >>> moveLeft >>> moveLeft >>> moveLeft >>> moveLeft >>> moveLeft >>> moveLeft >>> pop)

placeMarble :: Int -> (Scores, Board) -> Int -> (Scores, Board)
placeMarble nPlayers (scores, board) marble
     | marble `mod` 23 == 0 = let (removed, newBoard) = moveAndPop board
                                  player = marble `mod` nPlayers
                              in (adjust ((+) (removed + marble)) player scores, newBoard)
     | True = (scores, (moveRight >>> (insertRight marble)) $ board)

initScore :: Int -> Scores
initScore n = fromList $ map (\i -> (i, 0)) [0..n]

playUntil :: Int -> Int -> (Scores, Board)
playUntil nPlayers n = foldl' (placeMarble nPlayers) (initScore nPlayers, empty) [1..n]

highScore :: Scores -> Int
highScore s = foldl max 0 (elems s)

part1 :: IO ()
part1 = do
  (nPlayers, limit) <- parsePlayersAndLimit <$> loadInput
  putStr "Day 09 - Part 1: "
  putStrLn $ show $ highScore $ fst $ playUntil nPlayers limit

-- Note: can't run at the REPL due to stack overflow. Compile & run instead.
part2 :: IO ()
part2 = do
  (nPlayers, limit) <- parsePlayersAndLimit <$> loadInput
  putStr "Day 09 - Part 2: "
  putStrLn $ show $ highScore $ fst $ playUntil nPlayers (limit * 100)
