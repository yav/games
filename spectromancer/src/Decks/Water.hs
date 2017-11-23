module Decks.Water where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )

import CardIds
import GameMonad
import Deck
import CardTypes
import EffectAPI

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (water_merfolk_apostate,
        defaultCreature
          { onSummoned = \_ -> wizChangePower Caster Fire 2 })

    , (water_water_elemental,
        defaultCreature
          { onSummoned = \_ -> wizChangeLife Caster 10 })

  ]




