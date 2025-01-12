module Util.GridUtils.Coord
  ( Coord (..),
    Block,
    manhattanDistance,
    neighbors8,
    neighbors4,
    neighbors4Diagonal,
    deltas8,
    deltas4,
    deltas4Diagonal,
  )
where

data Coord = Coord Int Int deriving (Show)

type Block = (Coord, Coord)

instance Eq Coord where
  (==) :: Coord -> Coord -> Bool
  (Coord x1 y1) == (Coord x2 y2) = x1 == x2 && y1 == y2

instance Semigroup Coord where
  (<>) :: Coord -> Coord -> Coord
  (Coord x1 y1) <> (Coord x2 y2) = Coord (x1 + x2) (y1 + y2)

instance Monoid Coord where
  mempty :: Coord
  mempty = Coord 0 0

instance Ord Coord where
  compare :: Coord -> Coord -> Ordering
  compare (Coord x1 y1) (Coord x2 y2) = compare (x1, y1) (x2, y2)

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

deltas8 :: [Coord]
deltas8 =
  [ Coord x y
    | x <- [-1 .. 1],
      y <- [-1 .. 1],
      x /= 0 || y /= 0
  ]

deltas4 :: [Coord]
deltas4 =
  [ Coord x y
    | x <- [-1 .. 1],
      y <- [-1 .. 1],
      x /= 0 || y /= 0,
      abs x + abs y == 1
  ]

deltas4Diagonal :: [Coord]
deltas4Diagonal =
  [ Coord x y
    | x <- [-1 .. 1],
      y <- [-1 .. 1],
      x /= 0 || y /= 0,
      abs x + abs y == 2
  ]

neighbors8 :: Coord -> [Coord]
neighbors8 (Coord x y) = (Coord x y <>) <$> deltas8

neighbors4 :: Coord -> [Coord]
neighbors4 (Coord x y) = (Coord x y <>) <$> deltas4

neighbors4Diagonal :: Coord -> [Coord]
neighbors4Diagonal (Coord x y) = (Coord x y <>) <$> deltas4Diagonal