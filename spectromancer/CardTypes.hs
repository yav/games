{-# Language OverloadedStrings, RecordWildCards #-}
module CardTypes where

import           Data.Text(Text)
import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON(..), (.=))

type Category = Text
type Cards    = Map Category [Card]

data Card = Card
  { cardName        :: Text
  , cardDescription :: Text
  , cardImage       :: FilePath
  , cardCost        :: Int
  , cardEffect      :: CardEffect
  } deriving Show

isCreature :: Card -> Bool
isCreature c = case cardEffect c of
                 Creature {} -> True
                 Spell {}    -> False

isSpell :: Card -> Bool
isSpell = not . isCreature


data Who      = Caster | Opponent
                deriving (Eq,Ord,Show,Enum,Bounded)

theOtherOne :: Who -> Who
theOtherOne w =
  case w of
    Caster   -> Opponent
    Opponent -> Caster

data Location = Location { locWho :: Who, locWhich :: Int }
                deriving (Eq,Ord,Show)

-- | Note that this might return a location outside the board.
leftOf :: Location -> Location
leftOf l = l { locWhich = locWhich l - 1 }

-- | Note that this might return a location outside the board.
rightOf :: Location -> Location
rightOf l = l { locWhich = locWhich l + 1 }

oppositeOf :: Location -> Location
oppositeOf l = l { locWho = theOtherOne (locWho l) }

slotNum :: Int
slotNum = 6

slotsFor :: Who -> [ Location ]
slotsFor locWho = map mk (take slotNum [ 0 .. ])
  where mk locWhich = Location { .. }

isNeighbor :: Location -> Location -> Bool
isNeighbor loc1 loc2 = locWho loc1 == locWho loc2 && abs (locWhich loc1 - locWhich loc2) == 1

isOpposing :: Location -> Location -> Bool
isOpposing loc1 loc2 = locWho loc1 /= locWho loc2 && locWhich loc1 == locWhich loc2

cardTarget :: Card -> Target
cardTarget c =
  case cardEffect c of
    Creature {} -> TargetCasterBlank
    Spell tgt -> tgt

data CardEffect = Spell Target | Creature CreatureCard
  deriving Show

data Target = NoTarget          -- ^ Spell has no target
            | TargetOpponent's  -- ^ Spell needs an opponent's creature
            | TargetOpponent'sNormal
              -- ^ Spell needs an opponent's creature,
              -- which belongs to Air, Fair, Water, or Earth

            | TargetCaster's    -- ^ Spell needs a caster's creature
            | TargetCreature    -- ^ Spell needs someone's creature
            | TargetCasterBlank -- ^ Spell needs a caster's blank slot
  deriving Show

data CreatureCard = CreatureCard
  { creatureAttack      :: Maybe Int
  , creatureLife        :: Int
  } deriving Show

instance ToJSON Card where
  toJSON Card { .. } = JS.object fields
    where
    fields = special ++ common
    common = [ "name" .= cardName
             , "description" .= cardDescription
             , "image" .= cardImage
             , "cost" .= cardCost
             ]
    special = case cardEffect of
                Spell tgt -> [ "type" .= ("spell" :: Text)
                             , "target" .= tgt
                             ]
                Creature CreatureCard { .. } ->
                  [ "type" .= ("creature" :: Text)
                  , "attack" .= creatureAttack
                  , "life"   .=  creatureLife
                  ]

instance ToJSON Target where
  toJSON t =
    toJSON $
    case t of
      NoTarget                -> "none" :: Text
      TargetOpponent's        -> "opponent_creature"
      TargetOpponent'sNormal  -> "opponent_normal_creature"
      TargetCaster's          -> "catser_creature"
      TargetCasterBlank       -> "catser_blank"
      TargetCreature          -> "any_creature"

cardsToJSON :: Cards -> JS.Value
cardsToJSON cs = JS.object [ k .= v | (k,v) <- Map.toList cs ]




