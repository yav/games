module Util.Hex
  ( Loc(..)
  , Dir(..), allDirs
  , move
  , distance
  , turn
  , rotDist
  , neighbours

  , hexWidth, hexHeight
  , pixelToLoc, locToPixel
  , locPoints
  ) where

-- | A point on a hexagonal grid.
data Loc = Loc !Int !Int      -- (E, SE)
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
    NE -> Loc (x + n) (y - n)
    E  -> Loc (x + n) y
    SE -> Loc x (y + n)
    SW -> Loc (x - n) (y + n)
    W  -> Loc (x - n) y
    NW -> Loc x (y - n)

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


--------------------------------------------------------------------------------

sqrt_3 :: Double
sqrt_3 = sqrt 3


-- | Width of a hexagon with the given size (pointy bit up)
hexWidth :: Double {-^ length of edge -} -> Double
hexWidth s = s * sqrt_3

-- | Height of a hexagon with the given size (pointy bit up)
hexHeight :: Double {- ^ length of edge -} -> Double
hexHeight s = 2 * s

-- | Compute the location of the center of the given hexagon.
locToPixel :: Double {- ^ Hexgon edge -} -> Loc -> (Double,Double)
locToPixel s (Loc e' se') = (x * s,y * s)
  where
  x  = sqrt_3 * e + (sqrt_3 / 2) * se
  y  = (3/2) * se

  e  = fromIntegral e'
  se = fromIntegral se'

-- | Which location owns the given point.
pixelToLoc :: Double -> (Double,Double) -> Loc
pixelToLoc s (x,y) = roundLoc (e / s3) (se / s3)
  where e  = sqrt_3 * x - y
        se = 2 * y
        s3 = 3 * s

locPoints :: Double {- ^ Size of edge -} -> Loc -> [(Double,Double)]
locPoints s loc = [ (l, u), (x,y-s), (r, u), (r, d), (x,y+s), (l,d) ]
  where
  (x,y)  = locToPixel s loc
  l      = x - halfW
  r      = x + halfW
  u      = y - halfS
  d      = y + halfS
  halfS  = s / 2
  halfW  = (sqrt_3 / 2) * s

-- | Fractional location coordinates to closest location.
roundLoc :: Double -> Double -> Loc
roundLoc x z
  | err_x > err_y && err_x > err_z = Loc (bal ry rz) rz
  | err_y > err_z                  = Loc rx rz
  | otherwise                      = Loc rx (bal rx ry)
  where
  y = bal x z

  rx = round x
  ry = round y
  rz = round z

  err_x = err rx x
  err_y = err ry y
  err_z = err rz z

  err rt t = abs (t - fromIntegral rt)
  bal a b  = negate (a + b)



