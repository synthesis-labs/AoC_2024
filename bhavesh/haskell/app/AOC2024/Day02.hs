module AOC2024.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ filter id $ zipWith (||) normal reversed
  where
    normal = flip isReportSafe True <$> parseLevels input
    reversed = flip isReportSafe True . reverseList <$> parseLevels input

part2 :: T.Text -> Int
part2 input = length $ filter id $ arePermutatedReportsSafe <$> parseLevels input

arePermutatedReportsSafe :: [Int] -> Bool
arePermutatedReportsSafe arr = any (`isReportSafe` True) (generateLists 0 [] arr) || any ((`isReportSafe` True) . reverseList) (generateLists 0 [] arr)

parseLevels :: T.Text -> [[Int]]
parseLevels input = parseLevel <$> T.lines input
  where
    parseLevel inputLine = read . T.unpack <$> T.splitOn (T.pack " ") inputLine

generateLists :: Int -> [[Int]] -> [Int] -> [[Int]]
generateLists i acc arr
  | i == length arr = arr : acc
  | otherwise = generateLists (i + 1) newAcc arr
  where
    newAcc = deleteN i arr : acc

isReportSafe :: [Int] -> Bool -> Bool
isReportSafe [] acc = acc
isReportSafe [_] acc = acc
isReportSafe (x : y : xs) acc =
  if x <= y && (abs (x - y) <= 3) && x /= y
    then isReportSafe (y : xs) (acc && True)
    else isReportSafe (y : xs) (acc && False)

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x : xs) = reverseList xs ++ [x]

deleteN :: Int -> [a] -> [a]
deleteN _ [] = []
deleteN i (a : as)
  | i == 0 = as
  | otherwise = a : deleteN (i - 1) as
