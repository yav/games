module Hyperborea.Types where

import Data.Text(Text)
import Util.Bag(Bag)

-- | A collection of rules, only one of which may be used at a time.
data RuleGroup = RuleGroup
  { rules       :: [ Rule ]           -- ^ All alternatives
  , rulesVP     :: !Int               -- ^ Victory points
  }

data Raw            = Green | Red | Magenta | Orange | Yellow | Blue
                      deriving (Eq,Ord)

data Material       = Waste | Raw Raw
                      deriving (Eq,Ord)

data AnyMaterial    = AnyRaw | Material Material
                      deriving (Eq,Ord)

data Input          = Recall              -- ^ Recall an active avatar
                    | Use     AnyMaterial -- ^ Use some material
                    | Discard AnyMaterial -- ^ Discard a material
                      deriving (Eq,Ord)

data Action         = Move   | Fly
                    | Attack | RangedAttack
                    | Fortify
                    | ProgressDifferent | Progress1 | Progress2 | Progress3
                    | Buy
                    | Spawn | Clone
                    | GainAnyRaw | ChangeAnyRaw
                    | Gem
                    | Draw | Restore
                    | Espionage
                      deriving (Eq,Ord,Show,Bounded,Enum)

data AdjEffect      = LooseGem
                    | GainAction Action
                      deriving (Eq,Ord,Show)

data LongTermAction = WhenProduce Action Upgrade
                    | AtStart (Bag Action)

data Upgrade        = ConvertTo Action
                    | Generate (Bag Action)

data Rule           = Rule { ruleName       :: Text
                           , ruleInputs     :: Bag Input
                           , ruleProduces   :: RuleYield
                           }

data RuleYield  = Immediate [ ImmediateAction ] -- ^ Pick one of these actions
                | LongTerm  LongTermAction      -- ^ A long-term benefit

data ImmediateAction = ImmediateAction
  { playerActions   :: Bag Action     -- ^ Effects for player
  , adjacentActions :: Bag AdjEffect  -- ^ Effects on neighbours
  }



