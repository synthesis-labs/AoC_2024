module AOC2024.Day09
  ( part1,
    part2,
  )
where

import Data.Bifunctor qualified as BF
import Data.Char (digitToInt, isDigit)
import Data.List (group, sort, (!?))
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (\(i, n) acc -> acc + (n * i)) 0 numbs
  where
    numbs = BF.second read <$> V.filter (all isDigit . snd) (V.zip (V.fromList [(0 :: Int) ..]) sortedBlock)
    sortedBlock = reOrderDiskMap . diskMapToBlock $ parseDiskMap input

part2 :: T.Text -> Int
part2 input = 0

data DMT = File Char | Free Char deriving (Show)

reOrderDiskMap :: V.Vector String -> V.Vector String
reOrderDiskMap diskmap =
  if isInCorrectOrder diskmap
    then diskmap
    else reOrderDiskMap $ doASwap (fst dotAtStart) (fst digitAtEnd) diskmap
  where
    diskMapWithIndex = V.zip (V.fromList [(0 :: Int) ..]) diskmap
    dotAtStart = V.head $ V.filter ((== ".") . snd) diskMapWithIndex
    digitAtEnd = V.last $ V.filter (all isDigit . snd) diskMapWithIndex

doASwap :: Int -> Int -> V.Vector String -> V.Vector String
doASwap i j xs = left V.++ V.singleton elemJ V.++ middle V.++ V.singleton elemI V.++ right
  where
    elemI = xs V.! i
    elemJ = xs V.! j
    left = V.take i xs
    middle = V.take (j - i - 1) (V.drop (i + 1) xs)
    right = V.drop (j + 1) xs

isInCorrectOrder :: V.Vector String -> Bool
isInCorrectOrder arr = (== 1) . length $ filter (elem ".") $ V.group arr

diskMapToBlock :: V.Vector DMT -> V.Vector String
diskMapToBlock diskmap = V.fromList $ go [] 0 diskmap
  where
    go :: [[String]] -> Int -> V.Vector DMT -> [String]
    go acc i dm
      | null dm = concat acc
      | otherwise = case V.head dm of
          File x -> go (acc ++ [replicate (digitToInt x) (show i)]) (i + 1) (V.tail dm)
          Free x -> go (acc ++ [replicate (digitToInt x) "."]) i (V.tail dm)

parseDiskMap :: T.Text -> V.Vector DMT
parseDiskMap input = go V.empty File Free (T.unpack input)
  where
    go acc _ _ [] = acc
    go acc f g (x : xs) = go (acc V.++ V.singleton (f x)) g f xs
