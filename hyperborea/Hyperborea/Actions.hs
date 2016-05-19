{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
module Hyperborea.Actions where

import Prelude hiding ((/),(*),(>),(<))
import Data.Text(Text)
import Util.Bag

import Hyperborea.Types


class ToInputs a where
  toInputs :: a -> Bag Input

instance ToInputs Input where
  toInputs i = bagFromList [i]

instance ToInputs (Bag Input) where
  toInputs = id

instance ToInputs Raw where
  toInputs = toInputs . Raw

instance (ToInputs a, ToInputs b) => ToInputs (a,b) where
  toInputs (x,y) = bagUnion (toInputs x) (toInputs y)

instance (ToInputs a, ToInputs b, ToInputs c) => ToInputs (a,b,c) where
  toInputs (x,y,z) = toInputs (x,(y,z))

instance (ToInputs a, ToInputs b, ToInputs c,ToInputs d) => ToInputs (a,b,c,d)
  where toInputs (x,y,z,z') = toInputs (x,(y,z,z'))

instance ToInputs AnyMaterial where
  toInputs = toInputs . Use

instance ToInputs Material where
  toInputs = toInputs . Material

--------------------------------------------------------------------------------

class ToOutputs a where
  toOutputs :: a -> RuleYield

instance ToOutputs RuleYield where
  toOutputs = id

instance ToOutputs LongTermAction where
  toOutputs = LongTerm

instance ToOutputs [ImmediateAction] where
  toOutputs x = Immediate x

instance ToOutputs ImmediateAction where
  toOutputs x = Immediate [x]

instance ToOutputs [ Bag Action ] where
  toOutputs ass = Immediate [ ImmediateAction as bagEmpty | as <- ass ]

instance ToOutputs (Bag Action) where
  toOutputs as = Immediate [ ImmediateAction as bagEmpty ]

instance ToOutputs Action where
  toOutputs a = Immediate [ ImmediateAction (bagFromList [a]) bagEmpty ]

instance (ToOutputs a, ToOutputs b) => ToOutputs (a,b) where
  toOutputs (a,b) =
    case (toOutputs a, toOutputs b) of
      (Immediate xs, Immediate ys) ->
          Immediate [ ImmediateAction (bagUnion x y) (bagUnion as bs)
                      | ImmediateAction x as <- xs
                      , ImmediateAction y bs <- ys ]
      _ -> error "toOutputs: bad rule"


class ToActions a where
  toBag :: a -> Bag Action

instance ToActions (Bag Action) where
  toBag = id

instance ToActions Action where
  toBag a = bagFromList [a]

instance (ToActions a, ToActions b) => ToActions (a,b) where
  toBag (a,b) = bagUnion (toBag a) (toBag b)

class ToAdjEffect a where
  toAdjEffect :: a -> Bag AdjEffect

instance ToAdjEffect AdjEffect where
  toAdjEffect a = bagFromList [a]

instance ToAdjEffect Action where
  toAdjEffect a = bagFromList [GainAction a]


--------------------------------------------------------------------------------

infix 2 >
infix 3 -->
infix 4 =>+
infix 4 ===
infixr 5 /, &
infix 6 *

(-->) :: (ToInputs x, ToOutputs y) => x -> y -> Rule
xs --> ys = Rule { ruleName     = ""
                 , ruleInputs   = toInputs xs
                 , ruleProduces = toOutputs ys
                 }

(&) :: a -> b -> (a,b)
x & y = (x,y)

(>) :: Text -> Rule -> Rule
x > r = r { ruleName = x }

(/) :: (ToOutputs a, ToOutputs b) => a -> b -> [ ImmediateAction ]
xs / ys = case (toOutputs xs, toOutputs ys) of
            (Immediate a, Immediate b) -> a ++ b
            _ -> error "Invalid rule: only immaddiate rules may be or-ed"

(*) :: Int -> Action -> Bag Action
n * x = bagAdd n x bagEmpty

(=>+) :: ToActions a => Action -> a -> LongTermAction
x =>+ y = WhenProduce x (Generate (toBag y))

eachTurn :: ToActions a => a -> LongTermAction
eachTurn x = AtStart (toBag x)

(===) :: Action -> Action -> LongTermAction
x === y = WhenProduce x (ConvertTo y)

adj :: ToAdjEffect a => a -> RuleYield
adj a = Immediate [ ImmediateAction bagEmpty (toAdjEffect a) ]

--------------------------------------------------------------------------------

basic :: [RuleGroup]
basic =
  [ oneOf
      [ "G1"> (Green,AnyRaw)        --> 2 * Move
      , "G2"> (Green,Magenta)     --> Move & Spawn
      ]

  , oneOf
      [ "R1"> (Red,AnyRaw)          --> Attack / 2 * Fortify
      , "R2"> (Red,Green)         --> Attack & Move
      ]

  , oneOf
      [ "M1"> (Magenta,AnyRaw)      --> Spawn & Fortify
      , "M2"> (Magenta,Red)       --> Spawn & Attack
      ]

  , oneOf
      [ "O1"> (Orange,AnyRaw)       --> ProgressDifferent
      , "O2"> (Orange,Blue)       --> Progress2
      ]

  , oneOf
      [ "Y1"> (Yellow,AnyRaw)       --> Gem
      , "Y2"> (Yellow,Orange)     --> Gem & Progress1
      ]

  , oneOf
      [ "B1"> (Blue,AnyRaw,AnyRaw)    --> Buy
      , "B2"> (Blue,Yellow,AnyRaw)  --> Buy & Gem
      ]
  ]
  where
  oneOf xs = RuleGroup { rulesVP = 0, rules = xs }

tech :: Int -> Text -> Rule -> RuleGroup
tech n nm x  = RuleGroup { rulesVP = n
                         , rules   = [nm > x]
                         }

group1 :: [RuleGroup]
group1 =
  [ tech 1 "Archery"              $ (Red,Green)       --> RangedAttack
  , tech 1 "Armored Mastodons"    $ (Red,Green)       --> 2*Move & 2*Fortify
  , tech 2 "Beast Riding"         $ (Red,Red,Green)   --> 2*Attack & Move
  , tech 2 "Caravans"             $ (Green,AnyRaw)      --> 3*Move
  , tech 1 "Chariots"             $ (Red,Green)       --> Attack & 2*Move
  , tech 1 "Citadels"             $ Red               --> 2 * Fortify
  , tech 1 "Flying Giant Mounts"  $ (Green,AnyRaw)      --> 2 * Fly
  , tech 2 "Flying Mounts"        $ Green             --> Fly
  , tech 1 "Flying Ships"         $ Green             --> Move === Fly
  , tech 2 "Flying War Mounts"    $ (Red,Green,Green) --> Attack & 2*Fly
  , tech 1 "Nomadism"             $ (Green,Green)     --> Move =>+ Move
  , tech 2 "Phalanx"              $ (Red,AnyRaw)        --> Attack & Fortify
  , tech 1 "Wagons"               $ Green             --> 2*Move
  , tech 1 "Weapons Forging"      $ Red               --> Attack
  , tech 2 "Weapons Mastery"      $ (Red,Red,AnyRaw)    --> 2*Attack
  , tech 2 "Weapons Supremacy"    $ (Red,Red,AnyRaw)    --> Attack =>+ Attack
  ]


group2 :: [RuleGroup]
group2 =
  [ tech 1 "Arts"               $ (Yellow,Yellow,AnyRaw)  --> 2 * Gem
  , tech 1 "Borderland Cities"  $ Magenta               --> Clone & adj Spawn
  , tech 1 "Colonization"       $ (Magenta,Magenta)     --> Spawn === Clone
  , tech 1 "Huge Cities"        $ Magenta               --> Spawn
  , tech 1 "Marketplaces"       $ Yellow                --> Gem
  , tech 1 "Merchant Guilds"    $ (Yellow,Yellow,AnyRaw)  --> Gem =>+ Gem
  , tech 1 "Military Conquests" $ (Magenta,Yellow)      --> Spawn & Gem
  , tech 1 "Monasteries"        $ (Magenta,Orange,AnyRaw,Recall)
                                                  --> GainAnyRaw & 3 * Progress1
  , tech 0 "Outposts"           $ (Magenta,AnyRaw)        --> Clone
  , tech 2 "Peace Treaties"     $ Magenta               --> 2*Spawn & adj Spawn
  , tech 1 "Plundering"         $ (Yellow,Red,AnyRaw)     --> adj LooseGem
  , tech 2 "Recruitment"        $ (Magenta,Magenta)     --> Spawn =>+ Spawn
  , tech 1 "Sanctuaries"        $ (Magenta,Yellow,Orange,Recall)
                                                        --> Gem & 4 * Progress1
  , tech 1 "Temples"            $ (Magenta,Yellow,AnyRaw,Recall)
                                                        --> 2 * Gem
  , tech 2 "Trading Companies"  $ Yellow                --> 2 * Gem & adj Gem
  , tech 2 "Treasuries"         $ (Yellow,AnyRaw)         --> Gem
  ]


group3 :: [RuleGroup]
group3 =
  [ tech 1 "Alchemy"            $ Blue               --> Draw & ChangeAnyRaw
  , tech 2 "Architecture"       $ (Orange,Blue,AnyRaw) --> 2*Draw & 2*Progress1
  , tech 1 "Council of Elders"  $ (Orange,Orange)    --> Progress1 =>+ Progress1
  , tech 1 "Crossbows"          $ (Orange,Red)       --> RangedAttack
  , tech 0 "Diplomacy"          $ Blue               --> 3 * Draw & adj Draw
  , tech 1 "Expertise"          $ (Blue,Blue)        --> eachTurn Draw
  , tech 1 "Giant Libraries"    $ Orange             --> 2 * Progress1
  , tech 1 "Halls of Knowledge" $ (Blue,AnyRaw)        --> Buy
  , tech 1 "Infiltrations"      $ (Blue,Orange,AnyRaw) --> Espionage & Draw
  , tech 1 "Prosperity"         $ (Orange,Yellow)    --> 2 * Progress1 & Gem
  , tech 2 "Roads and Bridges"  $ (Orange,AnyRaw)
                                            --> 3 * Progress1 & adj Progress1
  , tech 1 "Siege Engines"      $ (Blue,Red)         --> RangedAttack
  , tech 1 "Splying"            $ (Blue,Orange)      --> Espionage
  , tech 2 "Undercover Agents"  $ (Blue,Orange,AnyRaw)
                                            --> Espionage & 2 * Progress1
  , tech 1 "Universities"       $ Orange             --> eachTurn Progress1
  , tech 1 "War Ships"          $ (Blue,Orange,Green)
                                                  --> RangedAttack & Move
  ]


group4 :: [RuleGroup]
group4 =
  [ tech 2 "Barricades"   $ (Red,Waste) --> 2 * Fortify
  , tech 1 "Bastions"     $ (Red,Orange,Waste) --> eachTurn (2 * Fortify)
  , tech 1 "Black Market" $ (AnyRaw,Waste) --> Restore
  , tech 2 "Bureaucracy"  $ (AnyRaw,Waste) --> Draw
  , tech 2 "Cartography"  $ (Green,Waste) --> Draw & Move
  , tech 1 "Code of Laws" $ (Blue,AnyRaw,Waste) --> Draw & Buy
  -- , tech 1 "Corruption"   $ (AnyRaw,Waste)
  ]


