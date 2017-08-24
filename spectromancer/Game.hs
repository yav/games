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

import Control.Lens( makeLenses, (^.), Lens',Traversal', at, non, (.~), (&)
                   , (%~), mapped
                   , IndexedTraversal', itraversed, indices, indexed, icompose )

import Util.Random(StdGen,Gen)

import CardTypes
import CardIds
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
  , _deckCardMods         :: [DeckCardMod]
    -- Temporary modifications
  } deriving Show

-- | Some sort of temporary modification to a deck card
data DeckCardMod = SkipNextAttack
  deriving (Show,Eq,Ord)

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
           , _deckCardMods = []
           }

deckCardName :: DeckCard -> Text
deckCardName c = c ^. deckCardOrig . cardName

-- | Add a new modification to card
deckCardAddMod :: DeckCardMod -> DeckCard -> DeckCard
deckCardAddMod m d = d & deckCardMods %~ (m:)

-- | Remove the mods for which the predicate returns 'True'.
deckCardRmMods :: (DeckCardMod -> Bool) {- ^ Which ones to remove -} ->
                  DeckCard -> DeckCard
deckCardRmMods p d = d & deckCardMods %~ filter (not . p)


isWall :: DeckCard -> Bool
isWall d = deckCardName d `elem` walls
  where walls = [ fire_wall_of_fire
                , illusion_wall_of_reflection
                , air_wall_of_lightning
                ]



-- This is in Gen to generate random starting powers...
newPlayer :: Text -> Deck -> Gen Player
newPlayer name deck =
  return Player { _playerName   = name
                , _playerLife   = 60
                , _playerActive = Map.empty
                , _playerDeck   = Map.mapWithKey dc deck
                , _playerPower  = Map.fromList [ (e,30) | e <- allElements ]
                }

  where dc e cs = map (newDeckCard e) cs

replaceCardList :: Text -> DeckCard -> [DeckCard] -> [DeckCard]
replaceCardList oldCardName newCard cards =
  case break ((== oldCardName) . deckCardName) cards of
    (before,_:after) -> before ++ newCard : after
    _                -> cards

-- | Replace a card in one of the players' decks.
replaceCard :: Who -> Text -> DeckCard -> Game -> Game
replaceCard who oldCardName newCard g =
  g & player who
    . playerDeck
    . at (newCard ^. deckCardElement)
    . mapped
    %~ replaceCardList oldCardName newCard


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

-- itraversed :: IndexedTraversal' k (Map k v) v

creatures :: IndexedTraversal' Slot Player DeckCard
creatures = playerActive . itraversed

playerCreaturesAt :: [Slot] -> Traversal' Player DeckCard
playerCreaturesAt ls = creatures . indices (`elem` ls)


-- Applicative f => (Who -> Player -> f Player) -> (Game -> f Game)
players :: IndexedTraversal' Who Game Player
players f g = mk <$> indexed f Caster p1 <*> indexed f Opponent p2
  where
  mk p1' p2' = g & curPlayer   .~ p1'
                 & otherPlayer .~ p2'

  p1 = g ^. curPlayer
  p2 = g ^. otherPlayer


creatureAt :: Location -> Lens' Game (Maybe DeckCard)
creatureAt l = player (locWho l) . creatureInSlot (locWhich l)

-- icompose :: (i -> j -> k) -> IndexedTraversal' i s b
--                           -> IndexedTraversal' j b a
--                           -> IndexedTraversal' k s a
gameCreatures :: IndexedTraversal' Location Game DeckCard
gameCreatures = icompose mk players creatures
  where mk p s = Location { locWho = p, locWhich = s }

creaturesAt :: [Location] -> IndexedTraversal' Location Game DeckCard
creaturesAt ls = gameCreatures . indices (`elem` ls)


deckCardLife :: Lens' DeckCard Int
deckCardLife = deckCard . creatureCard . creatureLife


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
    , "power"   .= JS.object [ Text.pack (show e) .= (c ^. elementPower e)
                               | e <- allElements ]
    , "active"  .= [ Map.lookup s (c ^. playerActive)
                                          | s <- take slotNum [ 0 .. ] ]
    ]

instance ToJSON Game where
  toJSON g = JS.object
    [ "current" .= (g ^. curPlayer)
    , "other"   .= (g ^. otherPlayer)
    ]

