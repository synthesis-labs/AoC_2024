module AOC2024.Day23
  ( part1,
    part2,
  )
where

import Data.List (intersect)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ pathOf3 g
  where
    g = parseConnections input

part2 :: T.Text -> Int
part2 input = 0

type Label = T.Text

type Graph = M.Map Label [Label]

pathOf3 :: Graph -> S.Set (S.Set Label)
pathOf3 g = go S.empty $ M.toList g
  where
    go :: S.Set (S.Set Label) -> [(Label, [Label])] -> S.Set (S.Set Label)
    go acc [] = acc
    go acc ((c1, cxns) : rest) =
      if not $ T.pack "t" `T.isPrefixOf` c1
        then go acc rest
        else go (go2 acc c1 cxns cxns) rest

    go2 :: S.Set (S.Set Label) -> Label -> [Label] -> [Label] -> S.Set (S.Set Label)
    go2 acc _ _ [] = acc
    go2 acc c1 cxns (c2 : rest) =
      case M.lookup c2 g of
        Nothing -> go2 acc c1 cxns rest
        Just cxns2 -> go2 (go3 c1 c2 acc (cxnsIntersection cxns2)) c1 cxns rest
      where
        cxnsIntersection = intersect cxns

    go3 :: Label -> Label -> S.Set (S.Set Label) -> [Label] -> S.Set (S.Set Label)
    go3 _ _ acc [] = acc
    go3 c1 c2 acc (c3 : xs) = go3 c1 c2 (S.insert newSet acc) xs
      where
        newSet = S.fromList [c1, c2, c3]

parseConnections :: T.Text -> Graph
parseConnections input = go M.empty $ T.lines input
  where
    go :: Graph -> [T.Text] -> Graph
    go acc [] = acc
    go acc (x : xs) =
      case T.splitOn (T.pack "-") x of
        [a, b] ->
          case (M.lookup a acc, M.lookup b acc) of
            (Nothing, Nothing) -> go (M.insert a [b] $ M.insert b [a] acc) xs
            (Nothing, Just bs) -> go (M.insert a [b] $ M.insert b (bs ++ [a]) acc) xs
            (Just as, Nothing) -> go (M.insert a (as ++ [b]) $ M.insert b [a] acc) xs
            (Just as, Just bs) -> go (M.insert a (as ++ [b]) $ M.insert b (bs ++ [a]) acc) xs
        _ -> go acc xs
