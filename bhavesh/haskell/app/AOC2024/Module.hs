module AOC2024.Module
  ( getParts,
  )
where

import AOC2024.Day01 qualified as Day01
import Model (Part (Part), Parts, errMsgParts)

getParts :: Int -> Parts
getParts day =
  case day of
    01 -> (Part Day01.part1, Part Day01.part2)
    _ -> errMsgParts errMsg
  where
    errMsg = "Day " <> show day <> " for 2024 has not been attempted yet"