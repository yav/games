{-# Language OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
{-# Language Rank2Types #-}
module Game where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS
import Data.Maybe(maybeToList)

import Control.Lens( makeLenses, (^.), Lens',Traversal', at, non)

import Util.Random(StdGen,Gen)

import CardTypes
import Deck


--------------------------------------------------------------------------------

data Game = Game
  { _curPlayer     :: Player
  , _otherPlayer   :: Player
  , _gameRNG       :: StdGen
  } deriving Show

data Player = Player
  { _playerLife    :: Int
  , _playerDeck    :: Map Element [DeckCard]
                      -- ^ The cards of the same element should be sorted
                      -- by cost, cheapest first.
  , _playerPower   :: Map Element Int
  , _playerActive  :: Map Slot DeckCard
  , _playerName    :: Text
  } deriving Show

type Slot = Int

-- | A card in a player's deck.
data DeckCard = DeckCard
  { _deckCardOrig         :: Card      -- ^ Unmodified deck card
  , _deckCardElement      :: Element   -- ^ Card's element
  , _deckCard             :: Card      -- ^ Current version of the card
  , _deckCardEnabled      :: Bool      -- ^ Is it currently playable
  , _deckCardAttackChange :: Int
    -- Temporary attack change, until the end of the current turn.
  } deriving Show

$(makeLenses ''Game)
$(makeLenses ''Player)
$(makeLenses ''DeckCard)

--------------------------------------------------------------------------------
-- Constructors

-- | Make a new inactive deck card.
newDeckCard :: Element -> Card -> DeckCard
newDeckCard el orig =
  DeckCard { _deckCard        = orig
           , _deckCardEnabled = False
           , _deckCardElement = el
           , _deckCardOrig    = orig
           , _deckCardAttackChange = 0
           }

deckCardName :: DeckCard -> Text
deckCardName c = c ^. deckCardOrig . cardName

-- This is in Gen to generate random starting powers...
newPlayer :: Text -> Deck -> Gen Player
newPlayer name deck =
  return Player { _playerName   = name
                , _playerLife   = 60
                , _playerActive = Map.empty
                , _playerDeck   = Map.mapWithKey dc deck
                , _playerPower  = Map.fromList [ (e,3) | e <- allElements ]
                }

  where dc e cs = map (newDeckCard e) cs


-- | A traversal that visits each card in a player's deck.
eachCard :: Traversal' Player DeckCard
eachCard = playerDeck   -- in the deck
         . traverse     -- for each element
         . traverse     -- for each card

player :: Who -> Lens' Game Player
player w =
  case w of
    Caster   -> curPlayer
    Opponent -> otherPlayer

elementPower :: Element -> Lens' Player Int
elementPower e = playerPower . at e . non 0

creatureInSlot :: Slot -> Lens' Player (Maybe DeckCard)
creatureInSlot s = playerActive . at s

creatureAt :: Location -> Lens' Game (Maybe DeckCard)
creatureAt l = player (locWho l) . creatureInSlot (locWhich l)

deckCardLife :: Lens' DeckCard Int
deckCardLife = deckCard . cardEffect . creatureCard . creatureLife

inhabitedSlots :: Game -> [Location] -> [(Location,DeckCard)]
inhabitedSlots g slots =
  [ (l,creature) | l <- slots, creature <- maybeToList (g ^. creatureAt l) ]






--------------------------------------------------------------------------------
-- JSON Serialization

jsElementMap :: ToJSON a => Map Element a -> JS.Value
jsElementMap = JS.object . map toField . Map.toList
  where toField (e,x) = Text.pack (show e) .= x

instance ToJSON DeckCard where
  toJSON c =
    JS.object [ "card"    .= (c ^. deckCard)
                                    -- XXX: add stats from original in desc.
              , "enabled" .= (c ^. deckCardEnabled)
              , "element" .= (c ^. deckCardElement)
              , "target"  .= (c ^. deckCard . cardTarget)
              ]

instance ToJSON Player where
  toJSON c = JS.object
    [ "name"    .= (c ^. playerName)
    , "life"    .= (c ^. playerLife)
    , "deck"    .= jsElementMap (c ^. playerDeck)
    , "power"   .= jsElementMap (c ^. playerPower)
    , "active"  .= [ Map.lookup s (c ^. playerActive)
                                          | s <- take slotNum [ 0 .. ] ]
    ]

instance ToJSON Game where
  toJSON g = JS.object
    [ "current" .= (g ^. curPlayer)
    , "other"   .= (g ^. otherPlayer)
    ]

