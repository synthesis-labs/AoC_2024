module AOC2024.Day11
  ( part1,
    part2,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ blinkMF 25 stones
  where
    stones = parseStones input

part2 :: T.Text -> Int
part2 input =
  foldl
    ( \acc (Memo steps _ _) ->
        if (steps !! (blinks - 1)) /= (-1)
          then acc + (steps !! (blinks - 1))
          else acc
    )
    0
    memo
  where
    blinks = 75
    memo = blinkMFMemo stones (createMemoTable blinks M.empty stones) (blinks, 0)
    stones = parseStones input

type Steps = [Int]

type Offset = Int

type Children = [Int]

data Memo = Memo Steps Offset Children deriving (Show)

createMemoTable :: Int -> M.Map Int Memo -> [Int] -> M.Map Int Memo
createMemoTable n = foldl (\acc x -> addToMemo n x 0 acc)

addToMemo :: Int -> Int -> Int -> M.Map Int Memo -> M.Map Int Memo
addToMemo blinks stone offset = M.insert stone (Memo (replicate blinks (-1)) offset [])

blinkMFMemo :: [Int] -> M.Map Int Memo -> (Int, Int) -> M.Map Int Memo
blinkMFMemo stones memo (blinks, t) =
  if t == blinks
    then memo
    else blinkMFMemo (addToStones stones newStones) newMemo (blinks, t + 1)
  where
    (newMemo, newStones) = getNewStones (reverse stones) (blinks, t) (memo, [])

addToStones :: [Int] -> [Int] -> [Int]
addToStones stones [] = stones
addToStones stones (x : xs) =
  if x `elem` stones
    then addToStones stones xs
    else addToStones (stones ++ [x]) xs

getNewStones :: [Int] -> (Int, Int) -> (M.Map Int Memo, [Int]) -> (M.Map Int Memo, [Int])
getNewStones [] _ (memo, newStones) = (memo, newStones)
getNewStones (stone : rest) (blinks, t) (memo, newStones) =
  case M.lookup stone memo of
    Just (Memo steps offset stonesChildren) ->
      case stonesChildren of
        [] -> getNewStones rest (blinks, t) (newNewMemo, newNewStones)
          where
            newNewStones = newStones ++ children
            children = applyRules stone
            newSteps = length children : drop 1 steps
            childrenNotInMemo = fst <$> filter snd ((\e -> (e, M.notMember e memo)) <$> children)
            newMemo = M.insert stone (Memo newSteps offset children) memo
            newNewMemo = foldl (\acc x -> addChildToMemo acc (blinks, t) x) newMemo childrenNotInMemo
        _ -> getNewStones rest (blinks, t) (newMemo, newStones)
          where
            newMemo = M.insert stone (Memo stoneNewStepsVal offset stonesChildren) memo
            stoneNewStepsVal =
              take (t - offset) steps
                ++ [sumOfChildren (t - offset - 1) memo 0 stonesChildren]
                ++ drop (t - offset + 1) steps
    Nothing -> getNewStones rest (blinks, t) (memo, newStones)

addChildToMemo :: M.Map Int Memo -> (Int, Int) -> Int -> M.Map Int Memo
addChildToMemo memo (blinks, t) child = M.insert child (Memo (replicate blinks (-1)) (t + 1) []) memo

sumOfChildren :: Int -> M.Map Int Memo -> Int -> [Int] -> Int
sumOfChildren _ _ childSum [] = childSum
sumOfChildren idx memo childSum (child : rest) = case M.lookup child memo of
  Just (Memo s _ _) -> sumOfChildren idx memo (childSum + (s !! idx)) rest
  Nothing -> sumOfChildren idx memo childSum rest

blinkMF :: Int -> [Int] -> [Int]
blinkMF 0 stones = stones
blinkMF n stones = blinkMF (n - 1) $ concatMap applyRules stones

applyRules :: Int -> [Int]
applyRules stone
  | stone == 0 = [1]
  | even (length (show stone)) = [read firstHalf, read secondHalf]
  | otherwise = [stone * 2024]
  where
    firstHalf = take (n `div` 2) strStone
    secondHalf = drop (n `div` 2) strStone
    n = length strStone
    strStone = show stone

parseStones :: T.Text -> [Int]
parseStones input = read . T.unpack <$> T.splitOn (T.pack " ") input
