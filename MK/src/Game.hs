{-# Language TemplateHaskell #-}
module Game where

import Control.Lens(makeLenses)
import Util.Bag

import Common
import Player
import Land


data Game = Game
  { _land         :: Land
  , _source       :: Bag Mana
  , _curPlayer    :: Player
  , _prevPlayers  :: [Player]
  , _nextPlayers  :: [Player]
  }

makeLenses ''Game

