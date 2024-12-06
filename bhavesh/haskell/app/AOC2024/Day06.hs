module AOC2024.Day06
  ( part1,
    part2,
  )
where

import Data.Maybe (isNothing)
import Data.Text qualified as T
import Data.Vector qualified as V
import Prelude hiding (Left, Right)

part1 :: T.Text -> Int
part1 input = length $ findVisited (grid, gridLen) Up start V.empty
  where
    gridLen = length $ T.lines input
    start = maybe (Coord (-1) (-1)) getCoord (V.findIndex (== '^') grid)
    grid = parseGrid input V.empty 0
    getCoord i = Coord (i `div` gridLen) (i `mod` gridLen)

part2 :: T.Text -> Int
part2 input = 0

data Coord = Coord Int Int deriving (Show, Ord)

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  (Coord x y) == (Coord x' y') = x == x' && y == y'

type Grid = V.Vector Char

type Visited = V.Vector Coord

data Direction = Up | Down | Left | Right

findVisited :: (Grid, Int) -> Direction -> Coord -> Visited -> Visited
findVisited (grid, gridLen) dir c visited =
  case grid V.!? newCoordPos of
    Just a -> case a of
      '#' -> findVisited (grid, gridLen) (changeDir dir) c visited
      _ ->
        if V.elem c visited
          then findVisited (grid, gridLen) dir newCoord visited
          else findVisited (grid, gridLen) dir newCoord (visited V.++ V.singleton c)
    Nothing ->
      if newNewVal == Just '#' || isNothing newNewVal
        then visited V.++ V.singleton c
        else findVisited (grid, gridLen) dir newCoord visited
  where
    newNewVal = grid V.!? getPos (applyDelta (changeDir dir) c)
    newCoordPos = getPos newCoord
    newCoord = applyDelta dir c
    getPos (Coord x y) = x * gridLen + y

changeDir :: Direction -> Direction
changeDir Up = Right
changeDir Right = Down
changeDir Down = Left
changeDir Left = Up

applyDelta :: Direction -> Coord -> Coord
applyDelta Up (Coord x y) = Coord (x - 1) y
applyDelta Down (Coord x y) = Coord (x + 1) y
applyDelta Left (Coord x y) = Coord x (y - 1)
applyDelta Right (Coord x y) = Coord x (y + 1)

parseGrid :: T.Text -> Grid -> Int -> Grid
parseGrid input acc gridLen =
  if gridLen == length (T.lines input)
    then acc
    else parseGrid input (acc V.++ V.fromList cols) (gridLen + 1)
  where
    cols = T.unpack $ T.lines input !! gridLen
