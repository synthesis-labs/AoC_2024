module AOC2024.Day14
  ( part1,
    part2,
  )
where

import Data.Bifunctor qualified as BF
import Data.List (group, sort)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Vector qualified as V
import Text.Parsec qualified as P
import Util.ParseHelpers (parseAoCInput)

part1 :: T.Text -> Int
part1 input = M.foldrWithKey (\k v acc -> if k /= Mid then acc * length v else acc) 1 quadrantsMap
  where
    quadrantsMap = mapOfQuadrants M.empty newPositionsWithQuadrants
    newPositionsWithQuadrants = getQuadrant ((wide - 1) `div` 2) ((tall - 1) `div` 2) <$> newPositions
    newPositions = getPosAfterX seconds wide tall <$> robotStats
    robotStats = parseRobotStats input
    (seconds, wide, tall) = (100, 101, 103)

part2 :: T.Text -> Int
part2 input = findSecondsForChristmasTree robotStats wide tall 0
  where
    xPositions = sort $ (\(Position x _) -> x) <$> newPositions
    (wide, tall) = (101, 103)
    newPositions = getPosAfterX 1 wide tall <$> robotStats
    robotStats = parseRobotStats input

data Position = Position Int Int deriving (Show, Ord)

instance Eq Position where
  (==) :: Position -> Position -> Bool
  (Position x y) == (Position x' y') = x == x' && y == y'

data Velocity = Velocity Int Int deriving (Show)

type RobotStat = (Position, Velocity)

type RobotStats = [RobotStat]

data Quadrant = Q1 | Q2 | Q3 | Q4 | Mid deriving (Eq, Show, Ord)

countToN :: [Position] -> Position -> Int -> Bool
countToN _ _ 0 = True
countToN l (Position x y) n = (Position (x + 1) y `elem` l) && countToN l (Position (x + 1) y) (n - 1)

isContiguous :: [Position] -> Bool
isContiguous [] = False
isContiguous (x : xs) =
  if c
    then c
    else isContiguous xs
  where
    c = countToN (x : xs) x 10

findSecondsForChristmasTree :: RobotStats -> Int -> Int -> Int -> Int
findSecondsForChristmasTree robotStats wide tall seconds =
  if isContiguous newPositions
    then seconds
    else findSecondsForChristmasTree robotStats wide tall (seconds + 1)
  where
    newPositions = getPosAfterX seconds wide tall <$> robotStats

mapOfQuadrants :: M.Map Quadrant [Position] -> [(Position, Quadrant)] -> M.Map Quadrant [Position]
mapOfQuadrants m [] = m
mapOfQuadrants m ((p, q) : ps) = mapOfQuadrants (M.insertWith (++) q [p] m) ps

getQuadrant :: Int -> Int -> Position -> (Position, Quadrant)
getQuadrant xMid yMid c@(Position x y)
  | x < xMid && y < yMid = (c, Q1)
  | x < xMid && y > yMid = (c, Q3)
  | x > xMid && y < yMid = (c, Q2)
  | x > xMid && y > yMid = (c, Q4)
  | otherwise = (c, Mid)

getPosAfterX :: Int -> Int -> Int -> (Position, Velocity) -> Position
getPosAfterX seconds wide tall (Position x y, Velocity vx vy) = Position (newX `mod` wide) (newY `mod` tall)
  where
    newX = x + (vx * seconds)
    newY = y + (vy * seconds)

parseRobotStats :: T.Text -> RobotStats
parseRobotStats input = parseAoCInput input robotStatsParser "robotStatsParser"
  where
    numParser =
      P.choice
        [ read <$> P.many1 P.digit,
          negate . read <$> (P.char '-' *> P.many1 P.digit)
        ]
    positionParser = Position <$> (P.string "p=" *> numParser) <*> (P.char ',' *> numParser)
    velocityParser = Velocity <$> (P.string " v=" *> numParser) <*> (P.char ',' *> numParser)
    robotStatParser = (,) <$> positionParser <*> velocityParser
    robotStatsParser = P.many1 $ robotStatParser <* P.optional P.newline
