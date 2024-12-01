module Main where

import Model (AoCDay, Year)
import Util.AOCHelpers (printAoCDay)

main :: IO ()
main = do
  putStrLn "Which year?"
  year <- readLn :: IO Int
  putStrLn "Which day?"
  day <- readLn :: IO Int
  printAoCDay (year, day)

printYear :: Year -> IO ()
printYear y = do
  putStrLn $ "---------Advent of Code " <> show y <> "---------"
  mapM_ printAoCDay (getAoCDays y)
  putStrLn ""

getAoCDays :: Year -> [AoCDay]
getAoCDays year = [(year, day) | day <- [1 .. 25]]