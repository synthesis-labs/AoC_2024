module AOC2024.Day06
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector qualified as V
import Prelude hiding (Left, Right)

part1 :: T.Text -> Int
part1 input = length . fst $ findVisited (grid, gridLen) Up start V.empty
  where
    gridLen = length $ T.lines input
    start = maybe (Coord (-1) (-1)) getCoord (V.findIndex (== '^') grid)
    grid = parseGrid input V.empty 0
    getCoord i = Coord (i `div` gridLen) (i `mod` gridLen)

part2 :: T.Text -> Int
part2 input = length $ V.filter snd loops
  where
    loops = (\g -> findVisited (g, gridLen) Up start V.empty) <$> newReplacedGrids
    newReplacedGrids = V.filter (V.elem '^') $ changeOneValue gridLen grid . fst <$> originalPath
    originalPath = fst $ findVisited (grid, gridLen) Up start V.empty
    gridLen = length $ T.lines input
    start = maybe (Coord (-1) (-1)) getCoord (V.findIndex (== '^') grid)
    grid = parseGrid input V.empty 0
    getCoord i = Coord (i `div` gridLen) (i `mod` gridLen)

data Coord = Coord Int Int deriving (Show, Ord)

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  (Coord x y) == (Coord x' y') = x == x' && y == y'

type Grid = V.Vector Char

type Visited = V.Vector (Coord, Direction)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

findVisited :: (Grid, Int) -> Direction -> Coord -> Visited -> (Visited, Bool)
findVisited (grid, gridLen) dir c visited =
  if not (inBounds newCoord gridLen)
    then (visited V.++ V.singleton (c, dir), False)
    else case grid V.!? newCoordPos of
      Just a -> case a of
        '#' -> findVisited (grid, gridLen) (changeDir dir) c visited
        _ ->
          if any (\e -> fst e == c) visited
            then
              if (c, dir) `elem` visited
                then (visited V.++ V.singleton (c, dir), True)
                else findVisited (grid, gridLen) dir newCoord visited
            else findVisited (grid, gridLen) dir newCoord (visited V.++ V.singleton (c, dir))
      Nothing -> (visited V.++ V.singleton (c, dir), False)
  where
    newCoordPos = getPos newCoord
    newCoord = applyDelta dir c
    getPos (Coord x y) = x * gridLen + y

inBounds :: Coord -> Int -> Bool
inBounds (Coord x y) gridLen = x >= 0 && x < gridLen && y >= 0 && y < gridLen

changeOneValue :: Int -> Grid -> Coord -> Grid
changeOneValue gridLen grid (Coord x y) = grid V.// [(x * gridLen + y, '#')]

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
