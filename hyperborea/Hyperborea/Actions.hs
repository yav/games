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

infix 1 >
infix 2 -->
infix 3 /
infix 4 *

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

(<) :: 
x < r = r

--------------------------------------------------------------------------------

basic :: [RuleGroup]
basic =
  [ ruleGroup
      [ "G1"> (Green,Wild)        --> 2 * Move
      , "G2"> (Green,Magenta)     --> (Move, Spawn)
      ]

  , ruleGroup
      [ "R1"> (Red,Wild)          --> Attack / 2 * Fortify
      , "R2"> (Red,Green)         --> (Attack,Move)
      ]

  , ruleGroup
      [ "M1"> (Magenta,Wild)      --> (Spawn,Fortify)
      , "M2"> (Magenta,Red)       --> (Spawn,Attack)
      ]

  , ruleGroup
      [ "O1"> (Orange,Wild)       --> ProgressDifferent
      , "O2"> (Orange,Blue)       --> Progress2
      ]

  , ruleGroup
      [ "Y1"> (Yellow,Wild)       --> Gem
      , "Y2"> (Yellow,Orange)     --> (Gem,Progress1)
      ]

  , ruleGroup
      [ "B1"> (Blue,Wild,Wild)    --> Buy
      , "B2"> (Blue,Yellow,Wild)  --> (Buy,Gem)
      ]
  ]

group1 :: [RuleGroup]
group1 = map mk
  [ "Archery">           (Red,Green)      --> RangedAttack
  , "Armored Mastodons"> (Red,Green)      --> (2*Move, 2*Fortify)
  , "Beast Riding">      (Red,Red,Green)  --> (2*Attack,Move)
  , "Caravans">          (Green,Wild)     --> 3*Move
  ]
  where
  mk x  = ruleGroup [x]







{-
example = "A" -: (A,B) --> 


data Rule     = Rule { ruleName       :: Text
                     , ruleInputs     :: Inputs
                     , ruleProduces   :: [ Bag Action ]
                       -- ^ The list accomodates alternatives
                     , ruleLongTerm   :: Bool
                     }
-}

