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
  toOutputs :: a -> RuleYield

instance ToOutputs RuleYield where
  toOutputs = id

instance ToOutputs LongTermAction where
  toOutputs = LongTerm

instance ToOutputs [ (Bag Action) ] where
  toOutputs = Immediate

instance ToOutputs Action where
  toOutputs a = Immediate [ bagFromList [a] ]

instance (ToOutputs a, ToOutputs b) => ToOutputs (a,b) where
  toOutputs (a,b) =
    case (toOutputs a, toOutputs b) of
      (Immediate xs, Immediate ys) ->
                                Immediate [ bagUnion x y | x <- xs, y <- ys ]
      _ -> error "toOutputs: bad rule"


class ToActions a where
  toBag :: a -> Bag Action

instance ToActions (Bag Action) where
  toBag = id

instance ToActions Action where
  toBag a = bagFromList [a]

instance (ToActions a, ToActions b) => ToActions (a,b) where
  toBag (a,b) = bagUnion (toBag a) (toBag b)


--------------------------------------------------------------------------------

infix 2 >
infix 3 -->
infix 4 /
infix 5 *

(-->) :: (ToInputs x, ToOutputs y) => x -> y -> Rule
xs --> ys = Rule { ruleName     = ""
                 , ruleInputs   = toInputs xs
                 , ruleProduces = toOutputs ys
                 }

(>) :: Text -> Rule -> Rule
x > r = r { ruleName = x }

(/) :: (ToOutputs a, ToOutputs b) => a -> b -> [ Bag Action ]
xs / ys = case (toOutputs xs, toOutputs ys) of
            (Immediate a, Immediate b) -> a ++ b
            _ -> error "Invalid rule: only immaddiate rules may be or-ed"

(*) :: Int -> Action -> [ Bag Action ]
n * x = [ bagAdd n x bagEmpty ]

generate :: ToActions a => a -> Upgrade
generate = Generate . toBag

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
  , vp 1 "Flying Ships" $ Green         --> WhenProduce Move (ConvertTo Fly)
  , vp 1 "Nomadism"     $ (Green,Green) --> WhenProduce Move (generate Move)
  , vp 2 "Weapons Supremacy"
      $ (Red,Red,Wild) --> WhenProduce Attack (generate Attack)
  ]
  where
  vp n nm x  = ruleGroup n [nm > x]
  xxx a = undefined :: Action





