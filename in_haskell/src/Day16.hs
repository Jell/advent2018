module Day16 (part1, part2) where
import Control.Category ((>>>))
import Data.Map as Map (Map, fromList, insert, (!), adjust, assocs)
-- import Text.ParserCombinators.Parsec
import Control.Monad.State.Lazy (State, execState, gets, modify)
import Data.Bits ((.&.), (.|.))
import Text.ParserCombinators.Parsec (GenParser, many, digit, string, char, sepBy, between, parse)
import Data.Set as Set (Set, fromList, intersection, (\\), elemAt)
import Data.Ord (comparing)
import Data.List (findIndices, sortBy)

loadInput :: IO (String)
loadInput = readFile "../inputs/day16.txt"

type Operands = (Int, Int, Int)
type Registers = Map Int Int
type OpID = Int
type TestCase = (Registers, OpID, Operands, Registers)
type Operation = Operands -> State Registers ()
type OpsIndex = Map Int Operation

initRegisters :: [Int] -> Registers
initRegisters = Map.fromList . zip [0..]

integer :: GenParser Char str Int
integer = read <$> many digit

opArgs :: GenParser Char str (OpID, Operands)
opArgs = do
  args <- integer `sepBy` (char ' ')
  return (head args, (args!!1,args!!2,args!!3))

testCase :: GenParser Char str TestCase
testCase = do
  string "Before: "
  state1 <- between (char '[') (char ']') (integer `sepBy` (string ", "))
  char '\n'
  (opID, args) <- opArgs
  char '\n'
  string "After:  "
  state2 <- between (char '[') (char ']') (integer `sepBy` (string ", "))
  char '\n'
  char '\n'
  return (initRegisters state1, opID, args, initRegisters state2)


inputData :: GenParser Char str ([TestCase], [(OpID, Operands)])
inputData = do
  cases <- many testCase
  char '\n'
  char '\n'
  ops <- many (opArgs <* (char '\n'))
  return (cases, ops)


parseInput :: String -> ([TestCase], [(OpID, Operands)])
parseInput = either (const ([],[])) id <$> parse inputData ""

register :: (Ord a) => a -> State (Map a b) b
register r = gets $ flip (!) r

value :: (Ord a) => b -> State (Map a b) b
value = pure

op :: State Registers Int -> (Int -> Int -> Int) -> State Registers Int -> State Registers Int -> State Registers ()
op target f a b = modify =<< insert <$> target <*> (f <$> a <*> b)

-- Addition:
--
--     addr (add register) stores into register C the result of adding register A and register B.
addr :: Operation
addr (a,b,c) = op (value c) (+) (register a) (register b)

--     addi (add immediate) stores into register C the result of adding register A and value B.
addi :: Operation
addi (a,b,c) = op (value c) (+) (register a) (value b)

--
-- Multiplication:
--
--     mulr (multiply register) stores into register C the result of multiplying register A and register B.
mulr :: Operation
mulr (a,b,c) = op (value c) (*) (register a) (register b)

--     muli (multiply immediate) stores into register C the result of multiplying register A and value B.
muli :: Operation
muli (a,b,c) = op (value c) (*) (register a) (value b)

--
-- Bitwise AND:
--
--     banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
banr :: Operation
banr (a,b,c) = op (value c) (.&.) (register a) (register b)

--     bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
bani :: Operation
bani (a,b,c) = op (value c) (.&.) (register a) (value b)

--
-- Bitwise OR:
--
--     borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
borr :: Operation
borr (a,b,c) = op (value c) (.|.) (register a) (register b)

--     bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
bori :: Operation
bori (a,b,c) = op (value c) (.|.) (register a) (value b)

--
-- Assignment:
--
--     setr (set register) copies the contents of register A into register C. (Input B is ignored.)
setr :: Operation
setr (a,b,c) = modify =<< insert <$> (value c) <*> (register a)

--     seti (set immediate) stores value A into register C. (Input B is ignored.)
seti :: Operation
seti (a,b,c) = modify =<< insert <$> (value c) <*> (value a)

--
-- Greater-than testing:
--
--     gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
gtir :: Operation
gtir (a,b,c) = op (value c) (\a b -> if a > b then 1 else 0) (value a) (register b)

--     gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
gtri :: Operation
gtri (a,b,c) = op (value c) (\a b -> if a > b then 1 else 0) (register a) (value b)

--     gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
gtrr :: Operation
gtrr (a,b,c) = op (value c) (\a b -> if a > b then 1 else 0) (register a) (register b)

--
-- Equality testing:
--
--     eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
eqir :: Operation
eqir (a,b,c) = op (value c) (\a b -> if a == b then 1 else 0) (value a) (register b)

--     eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
eqri :: Operation
eqri (a,b,c) = op (value c) (\a b -> if a == b then 1 else 0) (register a) (value b)

--     eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
eqrr :: Operation
eqrr (a,b,c) = op (value c) (\a b -> if a == b then 1 else 0) (register a) (register b)


ops :: [Operation]
ops = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

matchesCase :: TestCase -> Operation -> Bool
matchesCase (state1, opID, args, state2) op =
    state2 == (execState (op args) state1)

matchesMoreThan3 :: TestCase -> Bool
matchesMoreThan3 t = 3 <= (length $ filter (matchesCase t) ops)

solve1 :: [TestCase] -> Int
solve1 ts = length $ filter matchesMoreThan3 ts

part1 :: IO ()
part1 = do
  input <- loadInput
  let (cases, opsList) = parseInput input
  putStr "Day 16 - Part 1: "
  putStrLn $ show $ solve1 cases

constraints :: [TestCase] -> Map OpID (Set Int)
constraints ts = constraints' ts initMap
    where
      initMap =  Map.fromList $ zip [0..15] (repeat (Set.fromList [0..15]))

      constraints' :: [TestCase] -> Map Int (Set Int) -> Map Int (Set Int)
      constraints' [] mapping = mapping
      constraints' (t@(_, opID, _, _) : ts) mapping =
          constraints' ts (adjust (intersection matches) opID mapping)
              where matches = Set.fromList $ findIndices (matchesCase t) ops

resolve :: [(OpID, Set Int)] -> [(OpID, Set Int)]
resolve [] = []
resolve ((i, m) : xs)
    | length m == 1 = (i, m) : (resolve $ sortByConstraint $ removeMatch xs)
    where removeMatch = map (fmap (flip (\\) m))
          sortByConstraint = sortBy (comparing (length . snd))

inferOps :: [TestCase] -> OpsIndex
inferOps = constraints
         >>> assocs
         >>> sortBy (comparing (length . snd))
         >>> resolve
         >>> map (\ (opID, m) -> (opID, ops!!(elemAt 0 m)))
         >>> Map.fromList

compile :: OpsIndex -> [(OpID, Operands)] -> State Registers ()
compile opsIndex = mapM_ (\(opID, args) -> (opsIndex!opID) args)

part2 :: IO ()
part2 = do
  input <- loadInput
  let (cases, opsList) = parseInput input
  let opsIndex = inferOps cases
  let prog = compile opsIndex opsList
  let finalState = execState prog (initRegisters [0,0,0,0])
  putStr "Day 16 - Part 2: "
  putStrLn $ show $ finalState ! 0
