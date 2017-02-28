{-# LANGUAGE Safe, RecordWildCards, FlexibleInstances, OverloadedStrings #-}
module Rule
  ( Rule, globalRules
  , ruleOutput
  , ruleInput
  , useRule
  , ppRule
  , (===)
  , (&&&)
  , (-->)
  , (***)
  , requires
  , produces
  , onlyIf

  , Resource(..)
  , Target(..)
  , ChangeTerrainCost(..)
  , ChangeEnemy(..), EnemyCondition(..)
  , ChangeAttack(..)
  , ChangeUnit(..)

  , perEnemy
  , specialMove
  , EndMove(..)

  , ppResource
  , ppResources
  ) where

import Common
import Util.Bag

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Set(Set)
import qualified Data.Set as Set
import           Control.Monad (foldM)
import           Text.PrettyPrint



--------------------------------------------------------------------------------


data Rule = Rule
  { ruleName  :: Text
  , ruleIn    :: Bag Resource   -- ^ We consume these
  , ruleOut   :: Bag Resource   -- ^ We produce these
  }

instance Eq Rule where
  x == y = ruleName x == ruleName y

instance Ord Rule where
  compare x y = compare (ruleName x) (ruleName y)

useRule :: Rule -> Bag Resource -> Maybe (Bag Resource)
useRule Rule { .. } rAvail =
  bagUnion ruleOut `fmap` foldM rm rAvail (bagToListGrouped ruleIn)
  where
  rm rs (r,q) = bagRemove q r rs

ppRule :: Rule -> Doc
ppRule Rule { .. } = vcat [ text (Text.unpack ruleName) <> text ":"
                          , nest 2 (ppResources ruleIn)
                          , text "-->"
                          , nest 2 (ppResources ruleOut)
                          ]


-- Rule DSL --------------------------------------------------------------------

infix  1 ===
infixr 2 &&&
infix  3 -->
infix  4 ***

-- | Define an anonymous rule.
(-->) :: (Resources take, Resources make) => take -> make -> Rule
rIn --> rOut = Rule { ruleName = ""
                    , ruleIn   = bagFromList (resources rIn)
                    , ruleOut  = bagFromList (resources rOut)
                    }

-- | Set the name for a rule.
(===) :: Text -> Rule -> Rule
name === rule = rule { ruleName = name }


-- | Join two rules together.  The resulting rule has the requirements
-- for both rules, and the results of both rules.  The name is computed
-- by concatenating the two rules.
(&&&) :: Rule -> Rule -> Rule
r1 &&& r2 = Rule { ruleIn   = bagUnion (ruleIn r1) (ruleIn r2)
                 , ruleOut  = bagUnion (ruleOut r1) (ruleOut r2)
                 , ruleName = Text.append (ruleName r1) (ruleName r2)
                 }

class Resources r where
  resources :: r -> [Resource]

instance Resources Resource where
  resources r = [r]

instance Resources BasicMana where
  resources r = resources (BasicMana r)

instance Resources Mana where
  resources r = resources (ManaToken r)

instance Resources a => Resources [a] where
  resources = concatMap resources

instance (Resources a, Resources b) => Resources (a,b) where
  resources (x,y) = resources x ++ resources y

instance (Resources a, Resources b, Resources c) => Resources (a,b, c) where
  resources (x,y,z) = resources (x,(y,z))


(***) :: Resources r => Int -> r -> [Resource]
x *** y = [ c | r <- resources y, c <- replicate x r ]


-- | A rule that only requires the given resource.
requires :: Resources take => take -> Rule
requires rs = rs --> ([] :: [Resource])

-- | A rule that only produces the given resource.
produces :: Resources make => make -> Rule
produces rs = ([] :: [Resource]) --> rs

-- | This is used to make conditional rules.
onlyIf :: Resources cond => cond -> Rule
onlyIf rs = rs --> rs


ruleInput :: Rule -> [(Int,Resource)]
ruleInput Rule { .. } = [ (n,x) | (x,n) <- bagToListGrouped ruleIn ]

ruleOutput :: Rule -> [(Int,Resource)]
ruleOutput Rule { .. } = [ (n,x) | (x,n) <- bagToListGrouped ruleOut ]


globalRules :: [Rule]
globalRules =
  [ "Wings of Night" ===
      (WingsOfNight,Movement) --> (WingsOfNight,ChangeEnemy One NoAttack)
  ] ++
  [ sh ("Rebirth level " <> int n) ===
      n *** Rebirth --> ChangeUnit One [ UnitReadyLevel n ]
  | n <- [ 1 .. 3 ]
  ] ++
  [ sh ("Use " <> ppBasicMana m <> " crystal") ===
        ManaCrystal m --> m
  | m <- anyBasicMana
  ] ++
  [ sh ("Convert gold to " <> ppBasicMana m <> " mana") ===
    requires (IsTime Day) &&& (Gold --> m)
  | m <- anyBasicMana ]

  where
  sh x   = Text.pack (show x)

--------------------------------------------------------------------------------

data Resource =

    ManaToken Mana
  | ManaCrystal BasicMana

  | Movement
  | Influence
  | Attack AttackType Element
  | Block Element
  | SwiftBlock Element
  | Fame
  | GainReputation
  | LooseReputation               -- ^ Loose reputation at end of turn

  | Healing
  | GainWound

  | PerEnemy (Bag Resource)       -- ^ One of these per enemy

  | InAttackPhase
  | IsTime Time
  | OnTerrain Terrain

  | SpecialMove Int (Set Terrain) EndMove
    -- ^ Amount, terrains to avoid, what happens at the end

  | WingsOfNight
  | Rebirth

  | ChangeTerrainCost Terrain ChangeTerrainCost
  | ChangeEnemy Target ChangeEnemy
  | ChangeAttacks ChangeAttack
  | ChangeUnit Target [ChangeUnit]
    deriving (Eq,Ord)

perEnemy :: [Resource] -> Resource
perEnemy rs = PerEnemy (bagFromList rs)

specialMove :: Int -> [Terrain] -> EndMove -> Resource
specialMove n xs e = SpecialMove n (Set.fromList xs) e

data EndMove = EndMoveAttack | EndMoveSafe
               deriving (Eq,Ord)


data Target = Self | One | All
              deriving (Eq,Ord)

data ChangeEnemy =
    LooseArmor {- amount -} Int {- minimum -} Int
  | LooseFortifications
  | LooseCiteFortifications
  | LooseResistance Element
  | NoAttack
  | EnemyDestroy
  | EnemyIf [EnemyCondition] ChangeEnemy
  | EnemyAnd [ChangeEnemy]
    deriving (Eq,Ord)


data EnemyCondition =
    EnemyIsForitified
  | EnemyResists Element Bool
    deriving (Eq,Ord)

data ChangeAttack =
    AttackSetElement Element    -- ^ Set attacls
  | AttackSetType AttackType
    deriving (Eq,Ord)

data ChangeTerrainCost =
    DecreaseTo Int      -- ^ Decrease to the given value
  | DecreaseBy Int Int  -- ^ Decrease by amount, to minimum
    deriving (Eq,Ord,Show)

data ChangeUnit =
    UnitGainResistance Element
  | UnitGainWound
  | UnitReadyLevel Int       -- ^ Make ready, requires given level
  deriving (Eq,Ord)



-- Pretty Print

ppResource :: Resource -> Doc
ppResource = undefined {-resource =
  case resource of
    ManaToken m   -> ppMana m <+> text "token"
    ManaCrystal m -> ppBasicMana m <+> text "crystal"
    UsedCrystal m -> text "used" <+> ppBasicMana m <+> text "crystal"
    ManaSource m  -> ppMana m <+> text "in the source"
    ManaSourceFixed m -> ppMana m <+> text "source (fixed)"
    ManaDie       -> text "use of source"

    Movement      -> text "movement"
    Influence     -> text "influence"
    Attack at el -> elDoc <+> tyDoc <+> text "attack"
      where elDoc = ppElement el
            tyDoc = case at of
                      Melee  -> empty
                      Ranged -> text "ranged"
                      Siege  -> text "siege"
    Block e  -> ppElement e <+> text "block"
    SwiftBlock e -> ppElement e <+> text "swift block"
    Healing  -> text "healing"
    Fame     -> text "fame"
    BadReputation -> text "bad reputation"
    Reputation    -> text "reputation"

    DeedInHand x    -> text (show x) <+> text "card"
    DeedDestroyed x -> text (show x) <+> text "was destroyed"
    DeedDiscarded x -> text (show x) <+> text "was discarded"

    TimeIs t        -> text "it is" <+> ppTime t

    DrawDeed -> text "draw a card"

    IfInteracted rs -> ppResources (bagFromList rs) <+> text ", if interacted"
    Blocking x -> text "blocking" <+> text (Text.unpack x)
    RegainUsedCrystals -> text "regain used crystals"
    ChangeTerrainCost t c -> text "change terrain cost" -}


ppResources :: Bag Resource -> Doc
ppResources = vcat . map ppEntry . bagToListGrouped
  where ppEntry (r,x) = int x <+> ppResource r




