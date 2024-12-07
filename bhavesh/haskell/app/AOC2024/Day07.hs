module AOC2024.Day07
  ( part1,
    part2,
  )
where

import Control.Monad (replicateM, when)
import Data.List (intersperse, singleton)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ (\(Equation r _) -> r) <$> filter evalEquation equations
  where
    equations = parseEquations input

part2 :: T.Text -> Int
part2 input = 0

data Equation = Equation Int [Int] deriving (Show, Eq, Ord)

evalEquation :: Equation -> Bool
evalEquation (Equation result numbers) = result `elem` possibleResults
  where
    possibleResults = evaluate <$> intersperseCombinations ["+", "*"] (show <$> numbers)

evaluate :: [String] -> Int
evaluate tokens = go tokens 0
  where
    go [] acc = acc
    go [x] acc = acc + read x -- Add the last number
    go (x : "+" : y : xs) acc = go (show (read x + read y) : xs) acc
    go (x : "*" : y : xs) acc = go (show (read x * read y) : xs) acc

intersperseCombinations :: [String] -> [String] -> [[String]]
intersperseCombinations delimiters input = map (merge input) combinations
  where
    n = length input - 1
    combinations = replicateM n delimiters
    merge [] _ = []
    merge (x : xs) [] = [x]
    merge (x : xs) (d : ds) = x : d : merge xs ds

parseEquations :: T.Text -> [Equation]
parseEquations input = parseAoCInput input equationsParser "equationsParser"
  where
    numParser = read <$> P.many1 P.digit
    argumentParser = P.sepBy1 numParser (P.char ' ')
    equationParser = Equation <$> numParser <* P.string ": " <*> argumentParser
    equationsParser = P.many1 $ equationParser <* P.optional P.newline
