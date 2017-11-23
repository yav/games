module Decks.Earth where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )

import CardIds
import Cards
import GameMonad
import Game
import Deck
import EffectAPI

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (earth_giant_spider,
        defaultCreature
          { onSummoned = \l ->
              let fs = newDeckCard Earth
                         (getCard other_cards other_forest_spider)
              in summonLR l fs })
  ]




