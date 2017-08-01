{-# Language OverloadedStrings, RecordWildCards #-}
module State where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Text(Text)
import qualified Data.Text as Text
import Data.Aeson (ToJSON(..), (.=))
import qualified Data.Aeson as JS

import CardTypes
import Deck

data Player = Player
  { playerLife    :: Int
  , playerDeck    :: Deck
  , playerPower   :: Map Element Int
  , playerActive  :: Map Slot Card
  , playerName    :: Text
  } deriving Show



type Slot = Int


data Game = Game
  { curPlayer   :: Player
  , otherPlayer :: Player
  } deriving Show


slotNum :: Int
slotNum = 6

startLife :: Int
startLife = 60



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

