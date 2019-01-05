module Day17 (part1, part2) where
import Control.Applicative ((<|>))
import Text.ParserCombinators.Parsec (GenParser, many, digit, string, char, parse)
import Data.Set as Set (Set, fromList, unions, union, empty, toList, member, filter)
import Codec.Picture (generateImage, writePng, PixelRGB8(..))
import Data.Tuple (swap)
import Data.List (sort)
import Debug.Trace (trace)
import System.IO.Unsafe (unsafePerformIO)

loadInput :: IO (String)
loadInput = readFile "../inputs/day17.txt"

type Coord = (Int, Int)
type Boundaries = (Int, Int, Int, Int)

integer :: GenParser Char str Int
integer = read <$> many digit

verticalLine :: GenParser Char str (Set Coord)
verticalLine = do
  string "x="
  x <- integer
  string ", y="
  y1 <- integer
  string ".."
  y2 <- integer
  return $ fromList $ zip (repeat x) [y1..y2]

horizontalLine :: GenParser Char str (Set Coord)
horizontalLine = do
  string "y="
  y <- integer
  string ", x="
  x1 <- integer
  string ".."
  x2 <- integer
  return $ fromList $ zip [x1..x2] (repeat y)

landscape :: GenParser Char str (Set Coord)
landscape = do
  lines <- many $ (verticalLine <|> horizontalLine) <* char '\n'
  return $ unions lines

parseInput :: String -> Set Coord
parseInput = either (const Set.empty) id <$> parse landscape ""

getBoundaries :: Set Coord -> Boundaries
getBoundaries walls = (xMin, xMax, yMin, yMax)
      where
        wallsList = toList walls
        xMin = (minimum $ map fst wallsList) - 1
        xMax = (maximum $ map fst wallsList) + 1
        yMin = minimum $ map snd wallsList
        yMax = maximum $ map snd wallsList


render :: String -> Boundaries -> Set Coord -> Set Coord -> IO ()
render path (xMin, xMax, yMin, yMax) walls waters =
    writePng path $ generateImage pixelRenderer (1 + xMax - xMin) (1 + yMax)
   where pixelRenderer x y
             | member (x + xMin, y) waters = water
             | member (x + xMin, y) walls = wall
             | otherwise = nothing
         wall = PixelRGB8 128 128 128
         water = PixelRGB8 0 191 255
         nothing = PixelRGB8 255 248 220

fillVertical :: Boundaries -> Set Coord -> Coord -> [Coord]
fillVertical (_, _, _, yMax) walls (x, y)
    = zip (repeat x) $ takeWhile (\y' -> not $ member (x, y') walls) [y..yMax]

findBottom :: Boundaries -> Set Coord -> Coord -> [Coord]
findBottom (xMin, xMax, _, _) walls (x, y)
    = map swap $ zip (repeat (y + 1)) $ (takeWhile hitCheck [(max x xMin)..xMax]) ++ (takeWhile hitCheck [(min (x-1) xMax), (min (x-2) xMax)..xMin])
      where isFloor x' = member (x', y + 1) walls
            belowWall x'= member (x', y) walls
            hitCheck x' = (isFloor x') && (not $ belowWall x')

fillSplits :: Boundaries -> Set Coord -> [Coord] -> [Coord]
fillSplits boundaries walls [] = []
fillSplits boundaries walls [(x,y)] = fill boundaries walls (x, y)
fillSplits boundaries walls ((x, y):splits@((x',y'):_))
    = result ++ fillSplits boundaries walls splits
      where result = fill boundaries walls (x, y)
            nextUnderWater = member (x', y') (Set.fromList result)

fillUp :: Boundaries -> Set Coord -> [Coord] -> [Coord]
fillUp boundaries walls [] = []
fillUp boundaries@(xMin, xMax, yMin, yMax) walls bottom@((x,y):_)
    = case length newStarts of
        0 -> if endsWithWall newBottom then newBottom ++ fillUp boundaries walls newBottom else newBottom
        1 -> newBottom ++ fill boundaries (union walls $ fromList bottom) (head newStarts)
        otherwise -> newBottom ++ fillSplits boundaries (union walls $ fromList bottom) newStarts
    where newBottom = map swap $ zip (repeat (y - 1)) $ (takeWhile hitCheck [x..xMax]) ++ (takeWhile hitCheck [x-1,x-2..xMin])
          newStarts = Prelude.filter (\(x', _) -> (not $ aboveFloor x') && (not $ aboveWater x')) newBottom
          water = Set.fromList bottom
          aboveFloor x' = member (x', y) walls
          aboveWater x' = member (x', y) water
          hittingWall x'= member (x', y - 1) walls
          hitCheck x' = (aboveWater x' || aboveFloor x' || aboveFloor (x' - 1) || aboveFloor (x' + 1)) && (not $ hittingWall x')

          endsWithWall [] = False
          endsWithWall b =
              let bs = sort $ map fst b
                  xLeft = head bs
                  xRight = last bs
              in hittingWall (xLeft - 1) && hittingWall (xRight + 1)

fill :: Boundaries -> Set Coord -> Coord -> [Coord]
fill boundaries walls start =
    let vertical = fillVertical boundaries walls start
        bottom = findBottom boundaries walls (last vertical)
    in vertical ++ (fillUp boundaries walls bottom)

stepFill :: Boundaries -> Int -> Set Coord -> [Coord] -> Set Coord
stepFill boundaries@(xMin,xMax,yMin,yMax) height walls starts
         = if height >= yMax
           then result
           else union result $ stepFill boundaries (height + 200) walls newStarts
    where result = Set.unions $! map (Set.fromList . (fill (xMin, xMax, yMin, (min height yMax)) walls)) starts
          newStarts = toList $! Set.filter (\(x,y) -> (y == height) && (not (member (x,y+1) walls))) result


part1 :: IO ()
part1 = do
  walls <- parseInput <$> loadInput
  let boundaries@(xMin, xMax, yMin, yMax) = getBoundaries walls
  let water = Set.filter (\(x, y) -> y >= yMin) $ stepFill boundaries 200 walls [(500, 0)]
  render "day17_part1.png" boundaries walls water
  -- render "test.png" boundaries walls (Set.fromList $ stuckVert ++ stuckHoriz2)
  putStr "Day 17 - Part 1: "
  putStrLn $ show $ length water

part2 :: IO ()
part2 = do
  putStrLn "Day 17 - Part 2: "
