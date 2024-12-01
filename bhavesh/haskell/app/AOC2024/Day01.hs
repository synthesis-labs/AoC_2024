module AOC2024.Day01
  ( part1,
    part2,
  )
where

import Data.Bifunctor qualified
import Data.List (sort)
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = sum $ zipWith (\x y -> abs (x - y)) one two
  where
    (one, two) = parseLocationPairs input

part2 :: T.Text -> Int
part2 input = sum $ (\x -> occurrance x two * x) <$> one
  where
    (one, two) = parseLocationPairs input
    occurrance a = length . filter (a ==)

parseLocationPairs :: T.Text -> ([Int], [Int])
parseLocationPairs input = Data.Bifunctor.bimap sort sort . unzip $ createPairs <$> T.lines input
  where
    splitLine l = read . T.unpack <$> T.splitOn (T.pack "   ") l
    createPairs l = (head (splitLine l), last (splitLine l))
