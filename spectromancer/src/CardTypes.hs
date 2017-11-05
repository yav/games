{-# Language OverloadedStrings, RecordWildCards, TemplateHaskell, Rank2Types #-}
module CardTypes
  ( -- * Cards
    Card(..), Class
  , cardName
  , cardDescription
  , cardImage
  , cardCost
  , cardType
  , cardTarget
  , creatureCard

  , CardType(..)
  , isCreature, isSpell

  , CreatureCard(..)
  , creatureAttack
  , creatureLife

  , Target(..)

  -- * Players
  , Who(..)
  , theOtherOne

  -- * Locations
  , Location(..)
  , slotNum

  -- ** Construction
  , leftOf
  , rightOf
  , oppositeOf
  , slotsFor
  , allSlots

  -- ** Queries
  , isNeighbor
  , sameSide
  , onBoard
  , isOpposing
  )
  where

import           Data.Text(Text)
import qualified Data.Aeson as JS
import           Data.Aeson (ToJSON(..), (.=))
import           Control.Lens(Lens',lens,makeLenses,(^.))

type Class    = Text

data Card = Card
  { _cardName        :: Text
  , _cardDescription :: Text
  , _cardImage       :: FilePath
  , _cardCost        :: Int
  , _cardType        :: CardType
  , _cardTarget      :: Target
  } deriving Show

data CardType = Spell | Creature CreatureCard
  deriving Show

data CreatureCard = CreatureCard
  { _creatureAttack      :: Maybe Int
  , _creatureLife        :: Int
  } deriving Show

-- | Targets for cards.  This is most important for spells,
-- but some creatures also need to played in specific ways.
data Target = NoTarget          -- ^ Card has no target
            | TargetOpponent's  -- ^ Card needs an opponent's creature
            | TargetOpponent'sNormal
              -- ^ Card needs an opponent's creature,
              -- which belongs to Air, Fire, Water, or Earth

            | TargetCaster's    -- ^ Card needs a caster's creature
            | TargetCreature    -- ^ Card needs someone's creature
            | TargetCasterBlank -- ^ Card needs a caster's blank slot
  deriving Show


makeLenses ''Card
makeLenses ''CreatureCard

-- | Treat a card as a creature card.
-- WARNING: Fails if this is used on a spell.
creatureCard :: Lens' Card CreatureCard
creatureCard = cardType . lens getC setC
  where
  getC c =
    case c of
      Creature d -> d
      _          -> error "creatureCard: not a creature"

  setC _ c = Creature c


-- | Is this a crature card.
isCreature :: Card -> Bool
isCreature c = case c ^. cardType of
                 Creature {} -> True
                 Spell {}    -> False

-- | Is this a spell card.
isSpell :: Card -> Bool
isSpell = not . isCreature

--------------------------------------------------------------------------------

-- | Identifies the players.
data Who      = Caster      -- ^ The player who is taking the current turn.
              | Opponent    -- ^ The player who is not currently playing.
                deriving (Eq,Ord,Show,Read,Enum,Bounded)

-- | Compute the othe player.
theOtherOne :: Who -> Who
theOtherOne w =
  case w of
    Caster   -> Opponent
    Opponent -> Caster


--------------------------------------------------------------------------------

-- | A location on the battle field.
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

-- | How many slots are there on the battlefield.
slotNum :: Int
slotNum = 6

-- | Is this location on the board?
onBoard :: Location -> Bool
onBoard l = 0 <= slot && slot < slotNum
  where slot = locWhich l

-- | All locations for one of the player.
slotsFor :: Who -> [ Location ]
slotsFor locWho = map mk (take slotNum [ 0 .. ])
  where mk locWhich = Location { .. }

-- | All locations in the game.
allSlots :: [ Location ]
allSlots = slotsFor Caster ++ slotsFor Opponent

-- | Are these two locations next to each other, on the same side.
isNeighbor :: Location -> Location -> Bool
isNeighbor loc1 loc2 = sameSide loc1 loc2 &&
                       abs (locWhich loc1 - locWhich loc2) == 1

-- | Are these two locations on the same side.
sameSide :: Location -> Location -> Bool
sameSide l1 l2 = locWho l1 == locWho l2

-- | Are these two locations opposing each other.
isOpposing :: Location -> Location -> Bool
isOpposing loc1 loc2 = not (sameSide loc1 loc2) &&
                        locWhich loc1 == locWhich loc2

--------------------------------------------------------------------------------

instance ToJSON Location where
  toJSON l = JS.object [ "who" .= locWho l, "slot" .= locWhich l ]

instance ToJSON Who where
  toJSON w = case w of
               Caster   -> toJSON ("caster" :: Text)
               Opponent -> toJSON ("opponent" :: Text)

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
    special = case c ^. cardType of
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





