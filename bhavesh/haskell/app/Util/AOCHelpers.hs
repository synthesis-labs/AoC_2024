module Util.AOCHelpers
  ( printAoCDay,
  )
where

import AOC2024.Module qualified as AOC2024
import Data.Text.IO qualified as TIO
import Model (AoCAnswer, AoCDay, Part (Part), Parts, Timing (NoValue, Value), errMsgParts)
import System.IO.Error (tryIOError)
import Text.Printf (printf)

printAoCDay :: AoCDay -> IO ()
printAoCDay (year, day) = do
  (p1, p2) <-
    if not validated
      then pure (errMsgInvalidYear, errMsgInvalidYear)
      else runPart (year, day) filename
  printf $ "------ Day " <> d <> " ------"
  printf $ "\npart1: " <> p1
  printf $ "\npart2: " <> p2
  printf "\n\n"
  where
    d = padLeft (show day) '0' 2
    filename = "./data/" <> show year <> "-" <> d <> ".txt"
    padLeft s c n = replicate (n - length s) c <> s
    validated = year >= 2015 && day >= 1 && day <= 25
    errMsgInvalidYear = "Invalid AoC Day " <> show year <> "-" <> d

runPart :: AoCDay -> String -> IO (String, String)
runPart (year, day) filename = do
  (Part part1, Part part2) <- getAoCDayParts (year, day)
  inputTextResult <- tryIOError $ TIO.readFile filename
  pure $ case inputTextResult of
    Left _ -> (errMsgNoData, errMsgNoData)
    Right a -> (runPart' part1 a, runPart' part2 a)
  where
    runPart' p t = formatAoCAnswer (show $ p t, NoValue)
    errMsgNoData = "Input data file not found: " <> filename

getAoCDayParts :: AoCDay -> IO Parts
getAoCDayParts (year, day) =
  pure $ case year of
    2024 -> AOC2024.getParts day
    _ -> errMsgParts errMsgValidYear
  where
    errMsgValidYear = "Year " <> show year <> " has not been attempted yet"

formatAoCAnswer :: AoCAnswer -> String
formatAoCAnswer (p, t) =
  case t of
    Value a -> p <> " (" <> printf "%.9f" a <> " sec)"
    NoValue -> p
