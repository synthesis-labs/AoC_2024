module AOC2024.Day05
  ( part1,
    part2,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = sum $ getMiddle . snd <$> validUpdates
  where
    validUpdates = filter fst ((\u -> (isupdateValid rules u, u)) <$> updates)
    (rules, updates) = parseInput input

part2 :: T.Text -> Int
part2 input = sum $ getMiddle . reOrderUpdate rules 0 . snd <$> inValidUpdates
  where
    inValidUpdates = filter (not . fst) ((\u -> (isupdateValid rules u, u)) <$> updates)
    (rules, updates) = parseInput input

reOrderUpdate :: M.Map Int [Int] -> Int -> [Int] -> [Int]
reOrderUpdate rules i update =
  if i == length update
    then update
    else case M.lookup (update !! i) rules of
      Nothing -> reOrderUpdate rules (i + 1) update
      Just ruleValue ->
        if update /= newUpdate
          then reOrderUpdate rules 0 newUpdate
          else reOrderUpdate rules (i + 1) update
        where
          newUpdate = swapVals ruleValue (takeWhile (/= update !! i) update) i 0 update

swapVals :: [Int] -> [Int] -> Int -> Int -> [Int] -> [Int]
swapVals ruleValue beforeList i j update
  | j == length beforeList = update
  | (update !! j) `elem` ruleValue = swapVals ruleValue beforeList i (j + 1) (doASwap i j update)
  | otherwise = swapVals ruleValue beforeList i (j + 1) update

doASwap :: Int -> Int -> [Int] -> [Int]
doASwap i j arr
  | i == j = arr
  | i > j = doASwap j i arr
  | otherwise = left ++ [elemJ] ++ middle ++ [elemI] ++ right
  where
    elemI = arr !! i
    elemJ = arr !! j
    left = take i arr
    middle = take (j - i - 1) (drop (i + 1) arr)
    right = drop (j + 1) arr

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
