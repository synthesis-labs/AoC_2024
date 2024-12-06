module Day01 where
import Data.List (sort)

splitColumns :: String -> ([Int], [Int])
splitColumns input =
    let rows = map (map read . words) (lines input)
    in (map head rows, map last rows)

totalDifference :: [Int] -> [Int] -> Int
totalDifference xs ys = sum $ zipWith (\x y -> abs (x - y)) xs ys

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (== x)

occurenceTotal :: [Int] -> [Int] -> Int
occurenceTotal xs ys = sum [x * countOccurrences x ys | x <- xs]

run :: IO ()
run = do
  contents <- readFile "./data/day01"
  let (left, right) = splitColumns contents
  let answer1 = totalDifference (sort left) (sort right)
  let answer2 = occurenceTotal left right
  putStrLn $ "Answer Q1 => " ++ show answer1
  putStrLn $ "Answer Q2 => " ++ show answer2