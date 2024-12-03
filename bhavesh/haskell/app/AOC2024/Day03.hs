module AOC2024.Day03
  ( part1,
    part2,
  )
where

import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = sum $ sumPart1 0 <$> parseOps input

part2 :: T.Text -> Int
part2 input = sum $ sumPart2 True 0 <$> parseOps2 input

data Op
  = Mul Int Int
  | Do
  | Dont
  deriving (Show, Eq, Ord)

sumPart1 :: Int -> [Op] -> Int
sumPart1 s [] = s
sumPart1 s (op : ops) =
  case op of
    Mul x y -> sumPart1 (s + x * y) ops
    Do -> sumPart1 s ops
    Dont -> sumPart1 s ops

sumPart2 :: Bool -> Int -> [Op] -> Int
sumPart2 _ s [] = s
sumPart2 canSum s (op : ops) =
  case op of
    Mul x y ->
      if canSum
        then sumPart2 canSum (s + x * y) ops
        else sumPart2 canSum s ops
    Do -> sumPart2 True s ops
    Dont -> sumPart2 False s ops

parseOps :: T.Text -> [[Op]]
parseOps input = parseAoCInput input opsParser ""
  where
    numParser = read <$> P.many1 P.digit
    mulParser = Mul <$> (P.string "mul(" *> numParser <* P.string ",") <*> numParser <* P.string ")"
    voider = P.manyTill P.anyToken (P.try $ P.lookAhead mulParser)
    opParser = P.try voider *> P.sepEndBy (P.try mulParser) (P.try voider)
    opsParser = P.many1 opParser <* P.optional P.newline

parseOps2 :: T.Text -> [[Op]]
parseOps2 input = parseAoCInput input opsParser ""
  where
    numParser = read <$> P.many1 P.digit
    mulParser = Mul <$> (P.string "mul(" *> numParser <* P.string ",") <*> numParser <* P.string ")"
    doParser = Do <$ P.string "do()"
    dontParser = Dont <$ P.string "don't()"
    opParser =
      catMaybes
        <$> P.many1
          ( P.choice
              [ Just <$> P.try mulParser,
                Just <$> P.try doParser,
                Just <$> P.try dontParser,
                Nothing <$ P.anyChar
              ]
          )
    opsParser = P.many1 opParser <* P.optional P.newline
