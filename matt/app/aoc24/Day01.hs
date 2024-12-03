{-# LANGUAGE OverloadedStrings #-}

module Aoc24.Day01
  ( solvePart1,
  solvePart2,
  )
where

import Data.Text (Text, pack, replace, splitOn, unpack)
import Prelude
import Data.List (sort)

testInput :: [Text]
testInput = ["3   4", "4   3", "2   5", "1   3", "3   9", "3   3"]

readFileLines :: String -> IO [Text]
readFileLines fileName = do
  fileLines <- lines <$> readFile fileName
  let textFileLines = map pack fileLines
  pure textFileLines

splitIntoListsAndSort :: [Text] -> ([Int], [Int]) -> ([Int], [Int])
splitIntoListsAndSort [] (a,b) = (sort a, sort b)
splitIntoListsAndSort (x:xs) (a,b) =
    splitIntoListsAndSort xs acc
    where
        split = splitOn " " x
        firstInt = read (unpack $ head split) :: Int
        secondInt = read (unpack $ last split) :: Int
        acc = (a ++ [firstInt],b ++ [secondInt])


compareListsAndSum :: ([Int], [Int]) -> Int
compareListsAndSum (xs, xy) = sum $ map abs $ map (uncurry (-)) (zip xs xy) 

compareListsSimilarityAndSum :: ([Int], [Int]) -> Int
compareListsSimilarityAndSum (xs, xy) = 
  theSum
  where
    similarityScores = map (\x -> length $ filter (== x) xy) xs
    theSum = sum $ map (\(x, z) -> x * z) (zip xs similarityScores)

solvePart1 :: IO Int
solvePart1 = do
  contents <- readFileLines "./app/aoc24/aoc24_day01-input.txt"
  let splitLines = splitIntoListsAndSort contents ([],[])
  pure $ compareListsAndSum splitLines

solvePart2 :: IO Int
solvePart2 = do
  contents <- readFileLines "./app/aoc24/aoc24_day01-input.txt"
  let splitLines = splitIntoListsAndSort contents ([],[])
  pure $ compareListsSimilarityAndSum splitLines