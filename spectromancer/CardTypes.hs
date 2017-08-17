{-# Language OverloadedStrings, RecordWildCards, TemplateHaskell, Rank2Types #-}
module CardTypes where

import           Data.Text(Text)
import           Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON(..), (.=))
import           Control.Lens(Lens',lens,makeLenses,(^.))

type Category = Text
type Cards    = Map Category [Card]

data Card = Card
  { _cardName        :: Text
  , _cardDescription :: Text
  , _cardImage       :: FilePath
  , _cardCost        :: Int
  , _cardEffect      :: CardEffect
  , _cardTarget      :: Target
  } deriving Show

data CardEffect = Spell | Creature CreatureCard
  deriving Show

data CreatureCard = CreatureCard
  { _creatureAttack      :: Maybe Int
  , _creatureLife        :: Int
  } deriving Show

data Target = NoTarget          -- ^ Spell has no target
            | TargetOpponent's  -- ^ Spell needs an opponent's creature
            | TargetOpponent'sNormal
              -- ^ Spell needs an opponent's creature,
              -- which belongs to Air, Fair, Water, or Earth

            | TargetCaster's    -- ^ Spell needs a caster's creature
            | TargetCreature    -- ^ Spell needs someone's creature
            | TargetCasterBlank -- ^ Spell needs a caster's blank slot
  deriving Show

makeLenses ''Card
makeLenses ''CreatureCard

creatureCard :: Lens' CardEffect CreatureCard
creatureCard = lens getC setC
  where
  getC c =
    case c of
      Creature d -> d
      _          -> error "creatureCard: not a creature"

  setC _ c = Creature c


isCreature :: Card -> Bool
isCreature c = case c ^. cardEffect of
                 Creature {} -> True
                 Spell {}    -> False

isSpell :: Card -> Bool
isSpell = not . isCreature

--------------------------------------------------------------------------------

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

allSlots :: [ Location ]
allSlots = slotsFor Caster ++ slotsFor Opponent

isNeighbor :: Location -> Location -> Bool
isNeighbor loc1 loc2 = locWho loc1 == locWho loc2 &&
                              abs (locWhich loc1 - locWhich loc2) == 1

sameSide :: Location -> Location -> Bool
sameSide l1 l2 = locWho l1 == locWho l2

isOpposing :: Location -> Location -> Bool
isOpposing loc1 loc2 = not (sameSide loc1 loc2) &&
                        locWhich loc1 == locWhich loc2

instance ToJSON Card where
  toJSON c = JS.object fields
    where
    fields = special ++ common
    common = [ "name"        .= (c ^. cardName)
             , "description" .= (c ^. cardDescription)
             , "image"       .= (c ^. cardImage)
             , "cost"        .= (c ^. cardCost)
             , "target"      .= (c ^. cardTarget)
             ]
    special = case c ^. cardEffect of
                Spell -> [ "type" .= ("spell" :: Text) ]
                Creature ct ->
                  [ "type"   .= ("creature" :: Text)
                  , "attack" .= (ct ^. creatureAttack)
                  , "life"   .= (ct ^. creatureLife)
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




