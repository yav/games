module Player where

import Util.Bag

import BasicTypes
import Component


data Player = Player
  { playerResources :: Bag Resource
  , playerBuilt     :: [Component]
  , playerHand      :: [Component]
  , playerDeck      :: [Component]
  , playerDiscard   :: [Component]
  }



