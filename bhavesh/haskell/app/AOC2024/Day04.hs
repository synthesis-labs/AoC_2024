module AOC2024.Day04
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord (Coord), deltas8, neighbors4Diagonal)
import Util.GridUtils.Grid (GridInfo, parseGrid)

part1 :: T.Text -> Int
part1 input = countNumXMAS $ parseGrid id input

part2 :: T.Text -> Int
part2 input = countXmas's $ parseGrid id input

countNumXMAS :: GridInfo Char -> Int
countNumXMAS (grid, rows, cols) = foldr countXMASFold 0 xs
  where
    countXMASFold (c, _) acc = acc + length (filter isXMAS $ build8Paths c)
    isXMAS str = "SAMX" == str
    build8Paths c = buildPathStr c <$> deltas8
    buildPathStr c d = foldr strPathBuildFold "" $ buildPath4 c d
    strPathBuildFold (Coord x y) acc = snd (grid V.! (x * rows + y)) : acc
    buildPath4 c d = snd $ foldr (pathBuildFold d) (c, [c]) [1 .. length "XMAS" - 1]
    pathBuildFold d _ (c, p) = if inBounds $ c <> d then (c <> d, c <> d : p) else (c, p)
    inBounds (Coord x y) = x >= 0 && x < rows && y >= 0 && y < cols
    xs = V.filter ((== 'X') . snd) grid

countXmas's :: GridInfo Char -> Int
countXmas's (grid, rows, cols) = length $ V.filter isXmas $ buildXStr <$> as
  where
    isXmas str = str `elem` validXmas's
    validXmas's = ["MMSS", "MSMS", "SMSM", "SSMM"]
    buildXStr (c, _) = foldr strPathBuildFold "" $ neighbors4Diagonal c
    strPathBuildFold (Coord x y) acc = snd (grid V.! (x * rows + y)) : acc
    as = V.filter (\(c, val) -> val == 'A' && inBounds c) grid
    inBounds (Coord x y) = x >= 1 && x < rows - 1 && y >= 1 && y < cols - 1
