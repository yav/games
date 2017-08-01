{-# Language OverloadedStrings, RecordWildCards #-}
module State where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS

import Util.Random(Gen,StdGen, genRandFun, randSourceIO)

import CardTypes
import Deck

data Player = Player
  { playerLife    :: Int
  , playerDeck    :: Deck
  , playerPower   :: Map Element Int
  , playerActive  :: Map Slot Card
  , playerName    :: Text
  } deriving Show

newPlayer :: Text -> Deck -> Gen Player
newPlayer playerName playerDeck =
  do let playerPower = Map.fromList [ (e,3) | e <- allElements ]
     return Player { playerLife = startLife, playerActive = Map.empty, .. }

type Slot = Int


data Game = Game
  { curPlayer   :: Player
  , otherPlayer :: Player
  , gameRNG     :: StdGen
  } deriving Show


slotNum :: Int
slotNum = 6

startLife :: Int
startLife = 60

newGame :: StdGen -> (Text,Class) -> (Text,Class) -> Game
newGame rng (name1,class1) (name2,class2) =
  genRandFun rng $
    do (deck1, deck2) <- pickDecks class1 class2
       curPlayer   <- newPlayer name1 deck1
       otherPlayer <- newPlayer name2 deck2
       return $ \gameRNG -> Game { .. }

newGameIO :: (Text,Class) -> (Text,Class) -> IO Game
newGameIO p1 p2 =
  do gen <- randSourceIO
     return (newGame gen p1 p2)

--------------------------------------------------------------------------------
-- JSON Serialization

jsElementMap :: ToJSON a => Map Element a -> JS.Value
jsElementMap = JS.object . map toField . Map.toList
  where toField (e,x) = Text.pack (show e) .= x

instance ToJSON Player where
  toJSON Player { .. } = JS.object
    [ "name"    .= playerName
    , "life"    .= playerLife
    , "deck"    .= jsElementMap playerDeck
    , "power"   .= jsElementMap playerPower
    , "active"  .= [ Map.lookup s playerActive | s <- take slotNum [ 0 .. ] ]
    ]

instance ToJSON Game where
  toJSON Game { .. } = JS.object
    [ "current" .= curPlayer
    , "other"   .= otherPlayer
    ]

