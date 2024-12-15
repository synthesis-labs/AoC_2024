module AOC2024.Day10
  ( part1,
    part2,
  )
where

import Data.Bifunctor qualified as BF
import Data.Char (digitToInt)
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Vector qualified as V
import Prelude hiding (Left, Right)

part1 :: T.Text -> Int
part1 input = length $ M.filter (not . null) paths
  where
    paths = getAllPathsForAllStarts M.empty gridInfo startEndPairs
    startEndPairs = outerProduct starts ends
    starts = V.toList $ V.map fst $ V.filter (\(_, e) -> e == '0') grid
    ends = V.toList $ V.map fst $ V.filter (\(_, e) -> e == '9') grid
    gridInfo@(grid, _, _) = parseGrid input

part2 :: T.Text -> Int
part2 input = sum $ M.map length paths
  where
    paths = getAllPathsForAllStarts M.empty gridInfo startEndPairs
    startEndPairs = outerProduct starts ends
    starts = V.toList $ V.map fst $ V.filter (\(_, e) -> e == '0') grid
    ends = V.toList $ V.map fst $ V.filter (\(_, e) -> e == '9') grid
    gridInfo@(grid, _, _) = parseGrid input

data Coord = Coord Int Int deriving (Show, Ord)

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  (Coord x y) == (Coord x' y') = x == x' && y == y'

type Grid = V.Vector (Coord, Char)

type Path = V.Vector (Coord, Direction)

data Direction = Up | Down | Left | Right deriving (Show, Eq)

getAllPathsForAllStarts :: M.Map (Coord, Coord) [Path] -> (Grid, Int, Int) -> [(Coord, Coord)] -> M.Map (Coord, Coord) [Path]
getAllPathsForAllStarts acc _ [] = acc
getAllPathsForAllStarts acc gridInfo ((start, end) : rest) = getAllPathsForAllStarts newAcc gridInfo rest
  where
    newAcc = M.insert (start, end) (dfs gridInfo start (== end)) acc

-- Function to perform DFS
dfs :: (Grid, Int, Int) -> Coord -> (Coord -> Bool) -> [Path]
dfs gridInfo start isGoal = go S.empty V.empty start
  where
    -- Recursive DFS function
    go :: S.Set Coord -> Path -> Coord -> [Path]
    go visited path current
      | isGoal current = [path] -- If goal is reached, return the current path
      | otherwise = concat paths
      where
        visited' = S.insert current visited
        neighbors = getNeighbors gridInfo visited' current
        paths =
          mapMaybe
            ( \(coord, dir) ->
                if coord `S.member` visited'
                  then Nothing
                  else Just $ go visited' (path `V.snoc` (coord, dir)) coord
            )
            neighbors

getNeighbors :: (Grid, Int, Int) -> S.Set Coord -> Coord -> [(Coord, Direction)]
getNeighbors gridInfo visited (Coord x y) = filter isValid neighbors
  where
    (grid, rowLen, _) = gridInfo
    neighbors =
      [ (Coord (x - 1) y, Up),
        (Coord (x + 1) y, Down),
        (Coord x (y - 1), Left),
        (Coord x (y + 1), Right)
      ]
    isValid (coord, _) =
      case currentValue of
        Nothing -> False
        Just (_, cv) -> case nextValue of
          Nothing -> False
          Just (_, nv) ->
            digitToInt nv - digitToInt cv == 1
              && coordInGrid grid (Coord x y)
              && coordInGrid grid coord
              && coord `S.notMember` visited
      where
        currentValue = grid V.!? getPos (Coord x y)
        nextValue = grid V.!? getPos coord
        getPos (Coord x' y') = x' * rowLen + y'

-- Check if a coordinate is within the grid
coordInGrid :: Grid -> Coord -> Bool
coordInGrid grid coord = any (\(c, _) -> c == coord) grid

outerProduct :: (Monad m) => m a -> m b -> m (a, b)
outerProduct xs ys = do
  x <- xs -- for each x drawn from xs:
  y <- ys --   for each y drawn from ys:
  return (x, y) --      produce the (x,y) pair

parseGrid :: T.Text -> (Grid, Int, Int)
parseGrid input = (grid, rowLen grid, colLen grid)
  where
    grid = go (T.lines input) V.empty 0
    rowLen (g :: Grid) = V.maximum ((\(Coord x _) -> x) . fst <$> g) + 1
    colLen (g :: Grid) = V.maximum ((\(Coord _ y) -> y) . fst <$> g) + 1

    go :: [T.Text] -> Grid -> Int -> Grid
    go [] acc _ = acc
    go (x : xs) acc row = go xs (acc V.++ V.fromList columns) (row + 1)
      where
        columns = BF.first (Coord row) <$> zip [0 ..] (T.unpack x)
