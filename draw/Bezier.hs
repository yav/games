module Bezier (b1,b2,b3) where

type Point = (Double,Double)

infixl 6 .+.
infix  7 *.
(*.) :: Double -> Point -> Point
s *. (x,y) = pt (s * x) (s * y)

(.+.) :: Point -> Point -> Point
(.+.) (x0,y0) (x1,y1) = pt (x0+x1) (y0+y1)

pt :: Double -> Double -> Point
pt x y = x `seq` y `seq` (x,y)
--  where seq _ b = b

interp :: (Double -> Point) -> (Double -> Point) -> (Double -> Point)
interp f g = \t -> (1 - t) *. f t  .+.  t *. g t

b1 :: Point -> Point -> Double -> Point
b1 p0 p1 = interp (const p0) (const p1)

b2 :: Point -> Point -> Point -> Double -> Point
b2 p0 p1 p2 = interp (b1 p0 p1) (b1 p1 p2)

b3 :: Point -> Point -> Point -> Point -> Double -> Point
b3 p0 p1 p2 p3 = interp (b2 p0 p1 p2) (b2 p1 p2 p3)

reflect :: Point -> Point -> Point
reflect (x0,y0) (x1,y1) = (2*x1-x0,2*y1-y0)

jn :: Double -> (Double -> Point) -> (Double -> Point) -> Double -> Point
jn s f g = \t -> if t < s then f (inFst t) else g (inSnd t)
  where
  inFst t = t / s
  inSnd t = (t - s) / (1 - s)
