module AOC2024.Day08
  ( part1,
    part2,
  )
where

import Data.Bifunctor qualified as BF
import Data.List (nub, tails)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V

{-
925 is too low
-}
part1 :: T.Text -> Int
part1 input = length . nub $ foldr (flip (++)) [] (M.elems antiNodes)
  where
    antiNodes = M.map (getAntiNodes gridLen) pairsOfAntennas
    pairsOfAntennas = M.map pairs antennas
    gridLen = length $ T.lines input
    antennas = buildAntenna grid
    grid = parseGrid input

part2 :: T.Text -> Int
part2 input = length . nub $ foldr (flip (++)) [] (M.elems antiNodes)
  where
    antiNodes = M.map (getAllAntiNodes gridLen) pairsOfAntennas
    pairsOfAntennas = M.map pairs antennas
    gridLen = length $ T.lines input
    antennas = buildAntenna grid
    grid = parseGrid input

data Coord = Coord Int Int deriving (Show, Ord)

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  (Coord x y) == (Coord x' y') = x == x' && y == y'

type Grid = V.Vector (Coord, Char)

type Antenna = M.Map Char [Coord]

getAllAntiNodes :: Int -> [(Coord, Coord)] -> [Coord]
getAllAntiNodes gridLen coordPairs = go coordPairs []
  where
    go :: [(Coord, Coord)] -> [Coord] -> [Coord]
    go [] acc = acc
    go ((c1, c2) : rest) acc = [c1, c2] ++ go rest (acc ++ calculateAllAntiNodes gridLen (c1, c2))

getAntiNodes :: Int -> [(Coord, Coord)] -> [Coord]
getAntiNodes gridLen coordPairs = go coordPairs []
  where
    go :: [(Coord, Coord)] -> [Coord] -> [Coord]
    go [] acc = acc
    go ((c1, c2) : rest) acc = go rest (acc ++ calculateAntiNode gridLen (c1, c2))

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : ys) <- tails l, y <- ys]

calculateAllAntiNodes :: Int -> (Coord, Coord) -> [Coord]
calculateAllAntiNodes gridLen (c1@(Coord x1 y1), c2@(Coord x2 y2)) =
  if null antiNodesList
    then antiNodesList
    else
      antiNodesList
        ++ calculateAllAntiNodes gridLen (antinode1, c1)
        ++ calculateAllAntiNodes gridLen (c1, antinode2)
  where
    antiNodesList = filter (inBounds gridLen) [antinode1, antinode2]
    xDiff = abs $ x2 - x1
    yDiff = abs $ y2 - y1
    antinode1
      | x1 < x2 =
          if y1 < y2
            then Coord (x1 - xDiff) (y1 - yDiff)
            else Coord (x1 - xDiff) (y1 + yDiff)
      | y1 < y2 = Coord (x1 + xDiff) (y1 - yDiff)
      | otherwise = Coord (x1 + xDiff) (y1 + yDiff)
    antinode2
      | x1 < x2 =
          if y1 < y2
            then Coord (x2 + xDiff) (y2 + yDiff)
            else Coord (x2 + xDiff) (y2 - yDiff)
      | y1 < y2 = Coord (x2 - xDiff) (y2 + yDiff)
      | otherwise = Coord (x2 - xDiff) (y2 - yDiff)

calculateAntiNode :: Int -> (Coord, Coord) -> [Coord]
calculateAntiNode gridLen (c1@(Coord x1 y1), c2@(Coord x2 y2)) = filter (inBounds gridLen) [antinode1, antinode2]
  where
    xDiff = abs $ x2 - x1
    yDiff = abs $ y2 - y1
    antinode1
      | x1 < x2 =
          if y1 < y2
            then Coord (x1 - xDiff) (y1 - yDiff)
            else Coord (x1 - xDiff) (y1 + yDiff)
      | y1 < y2 = Coord (x1 + xDiff) (y1 - yDiff)
      | otherwise = Coord (x1 + xDiff) (y1 + yDiff)
    antinode2
      | x1 < x2 =
          if y1 < y2
            then Coord (x2 + xDiff) (y2 + yDiff)
            else Coord (x2 + xDiff) (y2 - yDiff)
      | y1 < y2 = Coord (x2 - xDiff) (y2 + yDiff)
      | otherwise = Coord (x2 - xDiff) (y2 - yDiff)

buildAntenna :: Grid -> Antenna
buildAntenna grid = M.filterWithKey (\k _ -> k /= '.') inverted
  where
    inverted = M.fromListWith (++) $ invertElem <$> V.toList grid
    invertElem (Coord x y, c) = (c, [Coord x y])

inBounds :: Int -> Coord -> Bool
inBounds gridLen (Coord x y) = x >= 0 && x < gridLen && y >= 0 && y < gridLen

parseGrid :: T.Text -> Grid
parseGrid input = go (T.lines input) V.empty 0
  where
    go :: [T.Text] -> Grid -> Int -> Grid
    go [] acc _ = acc
    go (x : xs) acc row = go xs (acc V.++ V.fromList columns) (row + 1)
      where
        columns = BF.first (Coord row) <$> zip [0 ..] (T.unpack x)
