module AOC2024.Day25
  ( part1,
    part2,
  )
where

import Data.Text qualified as T

part1 :: T.Text -> Int
part1 input = length $ filter id $ tryAKey <$> keyLockPairs
  where
    keyLockPairs = [(x, y) | x <- keys, y <- locks]
    (keys, locks) = parseKeysAndLocks input

part2 :: T.Text -> Int
part2 input = 0

tryAKey :: ([Int], [Int]) -> Bool
tryAKey (key, lock) = and $ zipWith (\x y -> x + y <= 5) key lock

parseKeysAndLocks :: T.Text -> ([[Int]], [[Int]])
parseKeysAndLocks input = (keys, locks)
  where
    (keys, locks) = (makeKeyOrLock <$> keyGrids, makeKeyOrLock <$> lockGrids)
    (keyGrids, lockGrids) = go [] [] grids
    grids = T.splitOn (T.pack "\n") <$> T.splitOn (T.pack "\n\n") input

    intLine :: [Int] -> T.Text -> [Int]
    intLine acc l
      | T.length l == 0 = acc
      | otherwise = case T.unpack $ T.take 1 l of
          "#" -> intLine (acc ++ [1]) (T.drop 1 l)
          "." -> intLine (acc ++ [0]) (T.drop 1 l)
          _ -> intLine acc (T.drop 1 l)

    makeKeyOrLock :: [T.Text] -> [Int]
    makeKeyOrLock grid = foldl (zipWith (+)) (replicate 5 (-1)) (intLine [] <$> grid)

    go :: [[T.Text]] -> [[T.Text]] -> [[T.Text]] -> ([[T.Text]], [[T.Text]])
    go ks lks [] = (ks, lks)
    go ks lks (x : xs) = case T.unpack $ last x of
      "#####" -> go (ks ++ [x]) lks xs
      _ -> go ks (lks ++ [x]) xs
