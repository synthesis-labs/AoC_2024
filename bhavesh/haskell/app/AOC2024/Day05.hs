module AOC2024.Day05
  ( part1,
    part2,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Data.List (permutations)

part1 :: T.Text -> Int
part1 input = sum $ getMiddle . snd <$> validUpdates
  where
    validUpdates = filter fst ((\u -> (isupdateValid rules u, u)) <$> updates)
    (rules, updates) = parseInput input

-- part2 :: T.Text -> Int
part2 input = inValidUpdates
  where
    inValidUpdates = filter (not . fst) ((\u -> (isupdateValid rules u, u)) <$> updates)
    (rules, updates) = parseInput input

getMiddle :: [Int] -> Int
getMiddle arr = arr !! (length arr `div` 2)

isupdateValid :: M.Map Int [Int] -> [Int] -> Bool
isupdateValid rules update = foldl aa True update
  where
    aa acc x = case M.lookup x rules of
      Nothing -> acc
      Just ruleValue -> case beforeList of
        [] -> True
        _ -> acc && not (any (`elem` ruleValue) beforeList)
      where
        beforeList = takeWhile (/= x) update

parseInput :: T.Text -> (M.Map Int [Int], [[Int]])
parseInput input = go M.empty [] (T.lines input)
  where
    strToInt = read . T.unpack

    putInMap :: M.Map Int [Int] -> Int -> Int -> M.Map Int [Int]
    putInMap m k v = case M.lookup k m of
      Nothing -> M.insert k [v] m
      Just x -> M.insert k (x ++ [v]) m

    go :: M.Map Int [Int] -> [[Int]] -> [T.Text] -> (M.Map Int [Int], [[Int]])
    go rules updates [] = (rules, updates)
    go rules updates (x : xs)
      | T.null x = go rules updates xs
      | '|' `T.elem` x = go (putInMap rules intBeforeBar intAfterBar) updates xs
      | otherwise = go rules (updates ++ [strToInt <$> T.splitOn (T.pack ",") x]) xs
      where
        intBeforeBar = strToInt (T.takeWhile (/= '|') x)
        intAfterBar = strToInt (T.takeWhileEnd (/= '|') x)
