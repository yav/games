{-# Language TypeFamilies #-}
module Lang
  ( Term
  , Color, red, green, blue
  , Shape, square, circle, triangle
  , Number
  , Item, color, shape, row, col, count
  , Prop
  , (==), (/=), (<), (<=), (>), (>=)
  , Logic
  , and, (&&), true
  , or, (||), false
  , not
  , (==>)
  , exists
  , forall
  , touching
  , prop
  , ($)
  ) where

import qualified Zendo as Z

import qualified Prelude as P
import Prelude ((.),map,error,($))

data Color
data Shape
data Number
data Item

newtype Term t = Term (P.Int -> Z.Term)
newtype Prop = P (P.Int -> Z.Prop)

red' :: Term Color
red' = Term (\_ -> Z.Color Z.Red)

blue' :: Term Color
blue' = Term (\_ -> Z.Color Z.Blue)

green' :: Term Color
green' = Term (\_ -> Z.Color Z.Green)

circle' :: Term Shape
circle' = Term (\_ -> Z.Shape Z.Circle)

square' :: Term Shape
square' = Term (\_ -> Z.Shape Z.Square)

triangle' :: Term Shape
triangle' = Term (\_ -> Z.Shape Z.Triangle)

red :: Term Item -> Prop
red x = color x == red'

blue :: Term Item -> Prop
blue x = color x == blue'

green :: Term Item -> Prop
green x = color x == green'

circle :: Term Item -> Prop
circle x = shape x == circle'

square :: Term Item -> Prop
square x = shape x == square'

triangle :: Term Item -> Prop
triangle x = shape x == triangle'

count :: (Term Item -> Prop) -> Term Number
count k = Term (\i -> let v = P.show i
                          P x = k (Term (\_ -> (Z.Var v)))
                      in Z.Count v (x (i P.+ 1)))



color :: Term Item -> Term Color
color (Term t) = Term (\i -> Z.ColorOf (t i))

shape :: Term Item -> Term Shape
shape (Term t) = Term (\i -> Z.ShapeOf (t i))

row :: Term Item -> Term Number
row (Term t) = Term (\i -> Z.RowOf (t i))

col :: Term Item -> Term Number
col (Term t) = Term (\i -> Z.ColOf (t i))

infix 4 ==, /=, >, <, >=, <=

(==) :: Term a -> Term a -> Prop
Term x == Term y = P (\i -> x i Z.:=: y i)

(/=) :: Term a -> Term a -> Prop
x /= y = not (x == y)

(>=) :: Term Number -> Term Number -> Prop
Term x >= Term y = P (\i -> case x i of
                              Z.Num 0 -> y i Z.:=: Z.Num 0
                              l -> l Z.:>=: y i)

(>) :: Term Number -> Term Number -> Prop
x > y = not (y >= x)

(<=) :: Term Number -> Term Number -> Prop
x <= y = y >= x

(<) :: Term Number -> Term Number -> Prop
x < y = y > x

touching :: Term Item -> Term Item -> Prop
touching (Term x) (Term y) = P (\i -> Z.Touching (x i) (y i))


class Logic t where
  and :: [t] -> t
  not :: t -> t
  exists :: t -> Prop

instance Logic Prop where
  and xs = P (\i -> Z.And [ f i | P f <- xs ])
  not (P x) = P (\i -> case x i of
                         Z.Not y -> y
                         y -> Z.Not y)
  exists p = p

instance (a ~ Term Item, Logic b) => Logic (a -> b) where
  and xs    = \a -> and [ f a | f <- xs ]
  not f     = \a -> not (f a)
  exists f  = count (\i -> exists (f i)) > 0

{-
Nested quantifiers DO NOT implicitly assert that the
items are different!

For example, this is false for all models containing
at least 1 item:

forall $ \x y -> touching x y

The reason is that an item is not considered to be touching itself.
-}

or :: Logic t => [t] -> t
or = not . and . map not

infixr 3 &&
infixr 2 ||
infixr 1 ==>

(&&) :: Logic t => t -> t -> t
x && y = and [x,y]

(||) :: Logic t => t -> t -> t
x || y = or [x,y]

true :: Logic t => t
true = and []

false :: Logic t => t
false = or []

(==>) :: Logic t => t -> t -> t
p ==> q = not p || q


forall :: Logic t => t -> Prop
forall p = not (exists (not p))

instance (a ~ Number) => P.Num (Term a) where
  fromInteger x = if x P.>= 0 then Term (\_ -> Z.Num (P.fromInteger x))
                              else error "No negative terms"
  (+) = error "Terms cannot be added."
  (*) = error "Terms cannot be multoiplied."
  abs = error "Cannot compute the absolute value of a term."
  signum = error "Cannot compute `signum` of a term."
  negate = error "Terms cannot be negative"

prop :: Prop -> Z.Prop
prop (P f) = f 0



