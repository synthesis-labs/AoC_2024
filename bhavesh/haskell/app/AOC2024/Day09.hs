module AOC2024.Day09
  ( part1,
    part2,
  )
where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV

part1 :: T.Text -> Int
part1 input = runST $ do
  diskmap <- parseDiskBlock input
  reOrderDiskMap diskmap
  MV.ifoldr (\i x acc -> acc + (getNum x * i)) 0 diskmap
  where
    getNum x = if all isDigit x then read x else 0

part2 :: T.Text -> Int
part2 input = getSum reOrderedDiskMap
  where
    reOrderedDiskMap = reOrderDiskMap' diskmap
    diskmap = parseDiskMap input

type MVector s = MV.MVector s String

getSum :: V.Vector (Int, String) -> Int
getSum = go 0 0
  where
    xx :: Int -> Int -> String -> Int -> Int -> V.Vector (Int, String) -> (Int, Int)
    xx rep i fid idx s dm
      | i == rep = (idx, s)
      | fid /= "." = xx rep (i + 1) fid (idx + 1) (s + (idx * read fid)) dm
      | otherwise = xx rep (i + 1) fid (idx + 1) s dm

    go :: Int -> Int -> V.Vector (Int, String) -> Int
    go acc idx dm =
      if null dm
        then acc
        else case dm V.!? 0 of
          Nothing -> acc
          Just (rep, fid) -> go newSum newIdx (V.tail dm)
            where
              (newIdx, newSum) = xx rep 0 fid idx acc dm

reOrderDiskMap' :: V.Vector (Int, String) -> V.Vector (Int, String)
reOrderDiskMap' diskmap = go ((V.length diskmap - 1) :: Int) diskmap
  where
    xx spcPtr (ptr, size, fid) dm =
      if spcPtr == ptr
        then dm
        else case dm V.!? spcPtr of
          Just (spcSize, spcFid) ->
            if spcFid /= "."
              then xx (spcPtr + 1) (ptr, size, fid) dm
              else
                if spcSize >= size
                  then xx ptr (ptr, size, fid) newDm
                  else xx (spcPtr + 1) (ptr, size, fid) dm
            where
              newDm = p1 V.++ p2 V.++ p3 V.++ p4 V.++ p5
              p1 = V.take spcPtr dm
              p2 = V.fromList [(size, fid), (spcSize - size, spcFid)]
              p3 = V.slice (spcPtr + 1) (ptr - spcPtr - 1) dm
              p4 = V.fromList [(size, ".")]
              p5 = V.drop (ptr + 1) dm
          Nothing -> dm

    go ptr acc =
      if ptr < 0
        then acc
        else case acc V.!? ptr of
          Just (_, ".") -> go (ptr - 1) acc
          Just (size, fid) -> go (ptr - 1) (xx 0 (ptr, size, fid) acc)
          Nothing -> acc

reOrderDiskMap :: MVector s -> ST s ()
reOrderDiskMap mvec = do
  let len = MV.length mvec
  go 0 (len - 1)
  where
    go focusIndex swappingIndex
      | focusIndex >= swappingIndex = return ()
      | otherwise = do
          focusVal <- MV.read mvec focusIndex
          swappingVal <- MV.read mvec swappingIndex
          case (focusVal, swappingVal) of
            (".", ".") -> go focusIndex (swappingIndex - 1)
            (".", _) -> do
              MV.swap mvec focusIndex swappingIndex
              go (focusIndex + 1) (swappingIndex - 1)
            _ -> go (focusIndex + 1) swappingIndex

parseDiskBlock :: T.Text -> ST s (MVector s)
parseDiskBlock input = do
  let size = calculateSize (T.unpack input)
  mvec <- MV.new size
  go 0 (0 :: Int) (0 :: Int) mvec (T.unpack input)
  return mvec
  where
    calculateSize :: String -> Int
    calculateSize [] = 0
    calculateSize (x : xs)
      | even (length xs) = digitToInt x + calculateSize xs
      | x == '0' = calculateSize xs
      | otherwise = digitToInt x + calculateSize xs

    go _ _ _ _ [] = return ()
    go offset i idx mvec (x : xs)
      | even idx = do
          let count = digitToInt x
          forM_ [0 .. count - 1] $ \j -> MV.write mvec (offset + j) (show i)
          go (offset + count) (i + 1) (idx + 1) mvec xs
      | x == '0' = go offset i (idx + 1) mvec xs
      | otherwise = do
          let count = digitToInt x
          forM_ [0 .. count - 1] $ \j -> MV.write mvec (offset + j) "."
          go (offset + count) i (idx + 1) mvec xs

parseDiskMap :: T.Text -> V.Vector (Int, String)
parseDiskMap input = V.fromList $ go [] (0 :: Int) (T.unpack input)
  where
    go acc _ [] = acc
    go acc i (x : xs) =
      if even i
        then go (acc ++ [(digitToInt x, show (i `div` 2))]) (i + 1) xs
        else go (acc ++ [(digitToInt x, ".")]) (i + 1) xs
