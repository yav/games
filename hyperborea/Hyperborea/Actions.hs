{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Hyperborea.Actions where

import Prelude hiding ((/),(*),(>),(<))
import Data.Text(Text)
import Util.Bag

import Hyperborea.Rules


class ToInputs a where
  toInputs :: a -> Inputs

inputsUnion :: Inputs -> Inputs -> Inputs
inputsUnion i j =
  Inputs { inputsWild     = inputsWild i + inputsWild j
         , inputsMaterial = bagUnion (inputsMaterial i) (inputsMaterial j)
         }

instance ToInputs Inputs where
  toInputs = id

instance ToInputs Material where
  toInputs a = Inputs { inputsWild = 0, inputsMaterial = bagFromList [a] }

instance ToInputs Raw where
  toInputs = toInputs . Raw

instance (ToInputs a, ToInputs b) => ToInputs (a,b) where
  toInputs (x,y) = inputsUnion (toInputs x) (toInputs y)

instance (ToInputs a, ToInputs b, ToInputs c) => ToInputs (a,b,c) where
  toInputs (x,y,z) = toInputs (x,(y,z))

data Wild = Wild

instance ToInputs Wild where
  toInputs _ = Inputs { inputsWild = 1, inputsMaterial = bagEmpty }

--------------------------------------------------------------------------------

class ToOutputs a where
  toOutputs :: a -> [ Bag Action ]

instance ToOutputs [ (Bag Action) ] where
  toOutputs = id

instance ToOutputs Action where
  toOutputs a = [ bagFromList [a] ]

instance (ToOutputs a, ToOutputs b) => ToOutputs (a,b) where
  toOutputs (a,b) = [ bagUnion x y | x <- toOutputs a, y <- toOutputs b ]




--------------------------------------------------------------------------------

infix 2 >
infix 3 -->
infix 4 /
infix 5 *

(-->) :: (ToInputs x, ToOutputs y) => x -> y -> Rule
xs --> ys = Rule { ruleName     = ""
                 , ruleInputs   = toInputs xs
                 , ruleProduces = toOutputs ys
                 , ruleLongTerm = False
                 }

(>) :: Text -> Rule -> Rule
x > r = r { ruleName = x }

longTerm :: Rule -> Rule
longTerm r = r { ruleLongTerm = True }

(/) :: (ToOutputs a, ToOutputs b) => a -> b -> [ Bag Action ]
xs / ys = toOutputs xs ++ toOutputs ys

(*) :: Int -> Action -> [ Bag Action ]
n * x = [ bagAdd n x bagEmpty ]


--------------------------------------------------------------------------------

basic :: [RuleGroup]
basic =
  [ basic
      [ "G1"> (Green,Wild)        --> 2 * Move
      , "G2"> (Green,Magenta)     --> (Move, Spawn)
      ]

  , basic
      [ "R1"> (Red,Wild)          --> Attack / 2 * Fortify
      , "R2"> (Red,Green)         --> (Attack,Move)
      ]

  , basic
      [ "M1"> (Magenta,Wild)      --> (Spawn,Fortify)
      , "M2"> (Magenta,Red)       --> (Spawn,Attack)
      ]

  , basic
      [ "O1"> (Orange,Wild)       --> ProgressDifferent
      , "O2"> (Orange,Blue)       --> Progress2
      ]

  , basic
      [ "Y1"> (Yellow,Wild)       --> Gem
      , "Y2"> (Yellow,Orange)     --> (Gem,Progress1)
      ]

  , basic
      [ "B1"> (Blue,Wild,Wild)    --> Buy
      , "B2"> (Blue,Yellow,Wild)  --> (Buy,Gem)
      ]
  ]
  where
  basic = ruleGroup 0

group1 :: [RuleGroup]
group1 =
  [ vp 1 "Archery"              $ (Red,Green)      --> RangedAttack
  , vp 1 "Armored Mastodons"    $ (Red,Green)      --> (2*Move, 2*Fortify)
  , vp 2 "Beast Riding"         $ (Red,Red,Green)  --> (2*Attack,Move)
  , vp 2 "Caravans"             $ (Green,Wild)     --> 3*Move
  , vp 1 "Chariots"             $ (Red,Green)      --> (Attack, 2 * Move)
  , vp 1 "Citadels"             $ Red              --> 2 * Fortify
  , vp 1 "Flying Giant Mounts"  $ (Green,Wild)     --> 2 * Fly
  , vp 2 "Flying Mounts"        $ Green            --> Fly
  , vp 2 "Flying War Mounts"    $ (Red,Green,Green) --> (Attack, 2 * Fly)
  , vp 2 "Phalanx"              $ (Red,Wild)        --> (Attack,Fortify)
  , vp 1 "Wagons"               $ Green             --> 2 * Move
  , vp 1 "Weapons Forging"      $ Red               --> Attack
  , vp 2 "Weapons Mastery"      $ (Red,Red,Wild)    --> 2 * Attack

  -- Continuous
  , vp 1 "Flying Ships" $ longTerm $ Green  --> xxx "Mov = Fly"
  , vp 1 "Nomadism"     $ longTerm $ (Green,Green) --> xxx "Mov -> +1 Mov"
  , vp 2 "Weapons Supremacy" $ longTerm $ (Red,Red,Wild) --> xxx "Attack -> +1 Attack"
  ]
  where
  vp n nm x  = ruleGroup n [nm > x]
  xxx a = undefined :: Action





