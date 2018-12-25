module Day08 (part1, part2) where
import Text.ParserCombinators.Parsec

loadInput :: IO (String)
loadInput = readFile "../inputs/day08.txt"

integer :: GenParser Char str Int
integer = read <$> many digit

data Tree = Node Int Int [Tree] [Int]
          deriving Show

treeParser :: GenParser Char str Tree
treeParser = do
  nc <- integer
  char ' '
  nm <- integer
  char ' '
  children <- count nc treeParser
  meta <- count nm (integer <* (char ' ' <|> char '\n'))
  return $ Node nc nm children meta

parseTree :: String -> Tree
parseTree = (either (const (Node 0 0 [] [])) id <$> parse treeParser "")

sumMeta :: Tree -> Int
sumMeta (Node _ _ children xs) = sum $ (xs <> (map sumMeta children))

rootValue :: Tree -> Int
rootValue (Node 0 _ _ xs) = sum xs
rootValue (Node n _ children xs) = sum $ map subValue inrange
       where inrange = filter ((>=) n) xs
             subValue i = rootValue (children !! (i - 1))

part1 :: IO ()
part1 = do
  i <- loadInput
  putStr "Day 08 - Part 1: "
  putStrLn $ show $ sumMeta $ parseTree i

part2 :: IO ()
part2 = do
  i <- loadInput
  putStr "Day 08 - Part 2: "
  putStrLn $ show $ rootValue $ parseTree i
