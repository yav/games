module Decks.Mechanical where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Monad (when)

import CardIds
import CardTypes
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (mechanical_steam_tank,
        defaultCreature
          { onSummoned = \_ ->
              damageCreatures (Effect AbilityDamage) 12 (slotsFor Opponent) })

    , (mechanical_dwarven_rifleman,
        defaultCreature
          { onSummonedOther = \cl sl ->
              when (locWho cl /= locWho sl)
                (damageCreature (Effect AbilityDamage) 4 sl) })



  ]




