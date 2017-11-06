{-# Language OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
{-# Language Rank2Types #-}
module Game
  (
  -- * Game state
    Game
  , gameNew
  , GameInit(..)
  , player
  , firstPlayerStart
  , gameRNG
  , replaceCard
  , playerCardNum
  , creatureAt
  , inhabitedSlots
  , playerPlayCardNum


  -- * Players
  , Player
  , newPlayer
  , playerName
  , playerClass
  , playerLife
  , playerDeck
  , eachCard
  , playerPower

  -- ** The summoned creatures
  , playerActive
  , creatureInSlot
  , creatures
  , playerCreaturesAt


  -- * Cards in Play
  , DeckCard
  , newDeckCard
  , deckCardOrig
  , deckCard

  , deckCardElement
  , deckCardEnabled
  , deckCardMods

  , deckCardName
  , deckCardLife
  , isWall

  -- ** Card modifications
  , deckCardAddMod
  , deckCardRmMods
  , DeckCardMod(..)
  )
  where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS
import Data.Maybe(maybeToList)

import Control.Lens( makeLenses, (^.), (.~), Lens',Traversal', at, non, (&)
                   , (%~), mapped
                   , IndexedTraversal', itraversed, indices)

import Util.Random(StdGen,Gen,genRandFun,randSource)

import CardTypes
import CardIds
import Deck

-- | Information needed for starting a new game.
data GameInit = GameInit
    { rngSeed :: Int
    , firstPlayer, secondPlayer :: (Text, Class)
    } deriving Show


data Game = Game
  { _curPlayer     :: Player
  , _otherPlayer   :: Player

  , _firstPlayerStart    :: Who
  -- ^ Indicates if the first player at the start of the game is
  -- the current active player, or if they are the opponent.

  , _gameRNG       :: StdGen
  -- ^ Used to resolve random events.
  } deriving Show

data Player = Player
  { _playerLife         :: Int
  , _playerDeck         :: Map Element [DeckCard]
                           -- ^ The cards of the same element should be sorted
                           -- by cost, cheapest first.
  , _playerPowerMap        :: Map Element Int
  , _playerActive       :: Map Slot DeckCard
  , _playerName         :: Text
  , _playerClass        :: Class
  , _playerPlayCardNum  :: Int
    -- ^ How many cards can we play. At the beginning of a turn,
    -- this usually gets set to 1, however sometimes it may become negative
    -- if the player is forced to skip playing a card.
  } deriving Show


-- | A card in a player's deck.
data DeckCard = DeckCard
  { _deckCardOrig         :: Card      -- ^ Unmodified deck card
  , _deckCardElement      :: Element   -- ^ Card's element
  , _deckCard             :: Card      -- ^ Current version of the card
  , _deckCardEnabled      :: Bool      -- ^ Is it currently playable
  , _deckCardMods         :: [DeckCardMod]
    -- ^ Temporary modifications
  } deriving Show

-- | Some sort of temporary modification to a deck card
data DeckCardMod = SkipNextAttack | AttackBoost Int
  deriving (Show,Eq,Ord)


$(makeLenses ''Game)
$(makeLenses ''Player)
$(makeLenses ''DeckCard)

-- | Setup a new game value.
-- Note that this game is in a "pre-initial" state.
-- For the complete initialization, have a look at 'newGame' in 'Turn'.
gameNew :: GameInit -> Game
gameNew gi =
  genRandFun (randSource (rngSeed gi)) $
    do let (name1, class1) = firstPlayer gi
           (name2, class2) = secondPlayer gi
       (deck1, deck2) <- pickDecks class1 class2
       p1 <- newPlayer name1 class1 deck1 Caster
       p2 <- newPlayer name2 class2 deck2 Opponent
       return $ \r -> Game { _curPlayer   = p1 & playerPlayCardNum .~ 1
                           , _otherPlayer = p2
                           , _firstPlayerStart = Caster
                           , _gameRNG     = r  }




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

-- | Name of the card.
deckCardName :: DeckCard -> Text
deckCardName c = c ^. deckCardOrig . cardName

-- | Add a new modification to card
deckCardAddMod :: DeckCardMod -> DeckCard -> DeckCard
deckCardAddMod m d = d & deckCardMods %~ (m:)

-- | Remove the mods for which the predicate returns 'True'.
deckCardRmMods :: (DeckCardMod -> Bool) {- ^ Which ones to remove -} ->
                  DeckCard -> DeckCard
deckCardRmMods p d = d & deckCardMods %~ filter (not . p)


-- | Is this card a wall.
isWall :: DeckCard -> Bool
isWall d = deckCardName d `elem` walls
  where walls = [ fire_wall_of_fire
                , illusion_wall_of_reflection
                , air_wall_of_lightning
                , control_damping_tower
                ]



-- | This is in Gen to generate random starting powers.
newPlayer :: Text -> Class -> Deck -> Who -> Gen Player
newPlayer name cls deck w =
  do initialMana <- genInitialMana (w == Caster) deck 
     return Player { _playerName        = name
                   , _playerLife        = 60
                   , _playerActive      = Map.empty
                   , _playerDeck        = Map.mapWithKey dc deck
                   , _playerPowerMap       = initialMana
                   , _playerClass       = cls
                   , _playerPlayCardNum = 0
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

-- | The game's players.
player :: Who -> Lens' Game Player
player w =
  case w of
    Caster   -> curPlayer
    Opponent -> otherPlayer

-- | The player's power in the given element.
playerPower :: Element -> Lens' Player Int
playerPower e = playerPowerMap . at e . non 0

creatureInSlot :: Slot -> Lens' Player (Maybe DeckCard)
creatureInSlot s = playerActive . at s


-- | Visit the creatures sommoned by the player.
creatures :: IndexedTraversal' Slot Player DeckCard
creatures = playerActive . itraversed

-- | Visit the creatures in the given slots.
playerCreaturesAt :: [Slot] -> Traversal' Player DeckCard
playerCreaturesAt ls = creatures . indices (`elem` ls)

-- | How many cards do we have left to play.
playerCardNum :: Who -> Lens' Game Int
playerCardNum w = player w . playerPlayCardNum



creatureAt :: Location -> Lens' Game (Maybe DeckCard)
creatureAt l = player (locWho l) . creatureInSlot (locWhich l)


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
    , "power"   .= JS.object [ Text.pack (show e) .= (c ^. playerPower e)
                               | e <- allElements ]
    , "active"  .= [ Map.lookup s (c ^. playerActive)
                                          | s <- take slotNum [ 0 .. ] ]
    ]

instance ToJSON Game where
  toJSON g = JS.object
    [ "current" .= (g ^. curPlayer)
    , "other"   .= (g ^. otherPlayer)
    , "left"    .= (g ^. firstPlayerStart)
    ]


