{-# Language GADTs, DataKinds, TypeOperators, KindSignatures #-}
module Language where

import Data.Kind(Type)
import Prelude hiding ((>))
import BasicTypes


data {-kind-} Thing = AComponent | AResource | ANat | ARival

type AComponent = 'AComponent
type AResource  = 'AResource
type ANat       = 'ANat
type ARival     = 'ARival
type Stack      = [Thing]
type a :> b     = (a :: Thing) ': (b :: Stack)
type Steps      = Step '[] '[]

infix  6 `times`
infixr 5 :>, `Then`, >
infix  4 |->

type (|->) = Step

data Step :: Stack -> Stack -> Type where
  This        :: a |-> AComponent :> a

  Nat         :: Int ->
                 a |-> ANat :> a

  Resource    :: Resource ->
                 a |-> AResource :> a


  ChooseC       :: a |-> AComponent :> a
  ChooseR       :: a |-> AResource :> a
  ChooseN       :: a |-> ANat :> a
  ChooseRi      :: a |-> ARival :> a

  Add           :: ANat :> ANat :> a |-> ANat :> a
  Different     :: Step (w :> w :> a) a

  Exhaust       :: AComponent :> a |-> a
  Restore       :: AComponent :> a |-> a
  Destroy       :: AComponent :> a |-> a
  Discard       :: AComponent :> a |-> a

  Gain          :: AResource :> a |-> a
  Spend         :: AResource :> a |-> a

  GainOn        :: AComponent :> AResource :> a |-> a
  SpendFrom     :: AComponent :> AResource :> a |-> a

  GainDiscount :: AResource :> a |-> a
                  -- a resource only usable during next build

  Build         :: AComponent :> a |-> a


  GetCost     :: AComponent :> a |-> ANat :> a
  GetCount    :: AResource :> ARival :> a |-> ANat :> a
  IsLifeForm  :: LifeForm -> AComponent :> a |-> a
  IsComponent :: ComponentType -> AComponent :> a |-> a
  Located     :: Location -> AComponent :> a |-> a
  Contains    :: AComponent :> AResource :> a |-> a


  Reorder     :: ANat :> a |-> a
  Draw        :: a |-> a

  AttackedFor :: ANat :> a |-> a

  NoCollect   :: a |-> a

  Rivals      :: a |-> a ->
                 a |-> a

  Resources   :: (AResource :> a |-> a) -> a |-> a

  Repeat      :: a         |-> a ->
                 ANat :> a |-> a

  Then        :: a |-> b ->
                 b |-> c ->
                 a |-> c

  Also        :: a :> b      |-> a :> a :> b
  WithPrev    :: a :> b :> c |-> b :> a :> c
  Done        :: a :> b      |-> b

(>) :: Step a b -> Step b c -> Step a c
(>) = Then

isNot :: Resource -> AResource :> a |-> AResource :> a
isNot r = Also > Resource r > Different

times :: Int -> a |-> a -> a |-> a
times n s = Nat n > Repeat s

attack :: Int -> a |-> a
attack n = Nat n > AttackedFor

gain :: Resource -> a |-> a
gain r = Resource r > Gain

spend :: Resource -> a |-> a
spend r = Resource r > Spend


