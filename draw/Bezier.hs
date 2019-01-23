module Bezier (Pt(..), Curve, b1,b2,b3, CurveStep(..), curve) where

data Pt  = Pt !Double !Double
  deriving Show

scale :: Double -> Pt -> Pt
scale x (Pt a b) = Pt (x * a) (x * b)
{-# INLINE scale #-}

add :: Pt -> Pt -> Pt
add (Pt a b) (Pt x y) = Pt (a + x) (b + y)
{-# INLINE add #-}

reflect :: Pt -> Pt -> Pt
reflect (Pt x0 y0) (Pt x1 y1) = Pt (2*x1-x0) (2*y1-y0)
{-# INLINE reflect #-}

-- | The domain of the input is between 0 and 1, inclusive.
type Curve = Double -> Pt

-- | Concatenate two curves.  The first input (between 0 and 1, inclusive)
-- indicates where the first curve ends: for values less then it, we use
-- the first curve, after we use the second one.
cat :: Double -> Curve -> Curve -> Curve
cat s f g = \t -> if t < s then f (inFst t) else g (inSnd t)
  where
  inFst t = t / s
  inSnd t = (t - s) / (1 - s)
{-# INLINE cat #-}

-- | Linear interpolation between two curves:
-- at 0 we behave like the first one, and at 1 like the second one.
interp :: Curve -> Curve -> Curve
interp f g = \t -> scale (1 - t) (f t) `add` scale t (g t)
{-# INLINE interp #-}

-- | A linear path.
b1 :: Pt -> Pt -> Curve
b1 p0 p1 = interp (const p0) (const p1)

-- | A quadratic Bezier curve.
b2 :: Pt -> Pt -> Pt -> Curve
b2 p0 p1 p2 = interp (b1 p0 p1) (b1 p1 p2)

-- | A cubic Bezier curve.
b3 :: Pt -> Pt -> Pt -> Pt -> Curve
b3 p0 p1 p2 p3 = interp (b2 p0 p1 p2) (b2 p1 p2 p3)


--------------------------------------------------------------------------------

data CurveStep = B2 Pt | B3 Pt Pt



curve :: Pt -> Pt -> [CurveStep] -> Curve
curve a b ps0 = foldr1 (cat step) segments
  where
  step = 1 / fromIntegral (length segments)

  segments = go True a b ps0

  go isFst p' p0 todo =
    case todo of
      []              -> []
      B2 p2 : more
        | isFst     -> b2 p' p0 p2 : go False p0 p2 more
        | otherwise -> b2 p0 i p2  : go False i  p2 more
      B3 p3 p4 : more
        | isFst     -> b3 p' p0 p3 p4 : go False p3 p4 more
        | otherwise -> b3 p0 i  p3 p4 : go False p3 p4 more
    where i = reflect p' p0




