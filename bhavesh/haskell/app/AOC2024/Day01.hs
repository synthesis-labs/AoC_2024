module AOC2024.Day01
  ( part1,
    part2,
  )
where

import Data.Bifunctor qualified as BF
import Data.List (sort)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

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
parseLocationPairs input = parseAoCInput input sortedLocationPairsParser "locationPairsParser"
  where
    numParser = read <$> P.many1 P.digit
    locationPairParser = (,) <$> (numParser <* P.string "   ") <*> numParser
    locationPairsParser = P.many1 (locationPairParser <* P.optional P.newline)
    sortedLocationPairsParser = BF.bimap sort sort . unzip <$> locationPairsParser
