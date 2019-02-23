module Bezier
  ( Pt(..), Coord(..), Coord2
  , path, Path(..), Sharp(..), Smooth(..), EndPath(..)
  ) where

data Pt = Pt !Double !Double
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




data Path       = From Coord2 Sharp | Join Path Double Path
data Smooth     = EndPt  Double        Coord2 EndPath
                | EndVec Double Coord2 Coord2 EndPath
data Sharp      = Toward Coord2 Smooth
data EndPath    = End
                | Smooth Smooth
                | Sharp  Sharp

data Coord      = To Double | By Double
type Coord2     = (Coord,Coord)

fromFlex :: Double -> Coord -> Double
fromFlex x f =
  case f of
    To x' -> x'
    By x' -> x + x'

fromCoord2 :: Pt -> Coord2 -> Pt
fromCoord2 (Pt x y) (x',y') = Pt (fromFlex x x') (fromFlex y y')

path :: Path -> Double -> Pt
path p = lkp tree
  where
  s = segments p
  (tree,_) = toT tot (length s) s
  tot = sum (map snd s)

data T = Node !Double T T
       | Leaf Curve

lkp :: T -> Double -> Pt
lkp t x =
  case t of
    Leaf c -> c x
    Node d l r
      | d < x -> lkp r (x - d)
      | otherwise -> lkp l x

toT :: Double -> Int -> [(Curve,Double)] -> (T,Double)
toT tot n ss =
  case ss of
    []      -> error "toT: []"
    [(c,t)] -> let l = t / tot
               in (Leaf (c . (* recip l)), l)
    _ -> let n2 = div n 2
         in case splitAt n2 ss of
              (as,bs) ->
                let (l,t1) = toT tot n2 as
                    (r,t2) = toT tot (n-n2) bs
                in (Node t1 l r, t1 + t2)



segments :: Path -> [(Curve,Double)]
segments pa = let ((p1,s),r) = flat pa []
              in sharp (fromCoord2 (Pt 0 0) p1) s r
  where
  flat p rest =
    case p of
      Join p1 d p2 -> let (s2,r) = flat p2 rest
                      in flat p1 ((d,s2) : r)
      From p1 s -> ((p1,s), rest)


  sharp a (Toward p1' step) rest =
    let p1 = fromCoord2 a p1'
    in case step of
         EndPt s p2' more ->
           let p2 = fromCoord2 p1 p2'
           in (b2 a p1 p2,s) : end p1 p2 more rest
         EndVec s p2' p3' more ->
           let p2 = fromCoord2 p1 p2'
               p3 = fromCoord2 p1 p3'
           in (b3 a p1 p2 p3,s) : end p2 p3 more rest


  end a b step rest =
    let i = reflect a b
    in case step of
         End -> case rest of
                  [] -> []
                  (s,(p1',Toward p2' more)) : rest' ->
                     let p1 = fromCoord2 b p1'
                         p2 = fromCoord2 b p2'
                         j  = scale (-1) $ reflect p2 p1
                     in (b3 b i j p1,s) : sharp p1 (Toward p2' more) rest'

         Smooth s ->
           case s of
             EndPt speed c' more ->
               let c = fromCoord2 b c'
               in (b2 b i c,speed) : end i c more rest
             EndVec speed c' d' more ->
               let c = fromCoord2 b c'
                   d = fromCoord2 b d'
              in (b3 b i c d,speed) : end c d more rest

         Sharp s -> sharp b s rest



