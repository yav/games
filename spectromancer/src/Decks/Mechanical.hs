module Decks.Mechanical where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )

import CardIds
import CardTypes
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (mechanical_steam_tank,
        defaultCreature
          { onSummoned = \_ -> damageCreatures Effect 12 (slotsFor Opponent) })



  ]




