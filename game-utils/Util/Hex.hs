module Util.Hex where

-- | A point on a hexagonal grid.
data Loc = Loc !Int !Int
  deriving (Eq,Ord,Show)

-- | A direction, which point towards an edge of a hexagon.
-- Hexagons are assumed to be with the pointy bit towards the north,
-- so @E@ and @W@ are edges, while @N@ and @S@ are verices.
data Dir = NE | E | SE | SW | W | NW
  deriving (Eq,Ord,Show,Enum,Bounded)

-- | All directions.
allDirs :: [Dir]
allDirs = [ minBound .. maxBound ]

-- | Move the given amount in the given direction.
move :: Int -> Dir -> Loc -> Loc
move n d (Loc x y) =
  case d of
    NE -> Loc x (y + n)
    E  -> Loc (x + n) y
    SE -> Loc (x + n) (y - n)
    SW -> Loc x (y - n)
    W  -> Loc (x - n) y
    NW -> Loc (x - n) (y + n)

-- | The length of the shortest path from one location to the next.
distance :: Loc -> Loc -> Int
distance (Loc x1 y1) (Loc x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Compute a new direction. Positive is clockwise.
turn :: Int -> Dir -> Dir
turn n = toEnum . (`mod` 6) . (+ n) . fromEnum

-- | The rotational distance between two directions.
-- @turn (rotDist d1 d2) d1 == d2@.
rotDist :: Dir -> Dir -> Int
rotDist d1 d2 = fromEnum d2 - fromEnum d1

-- | The locations adjecent to a location.
neighbours :: Loc -> [Loc]
neighbours l = [ move 1 d l | d <- allDirs ]


