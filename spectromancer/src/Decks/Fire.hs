module Decks.Fire where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Data.List(delete)
import           Control.Lens ((.~), mapped)

import CardIds
import Game
import GameMonad
import CardTypes
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (fire_fire_drake,
        defaultCreature
          { onSummoned = \l ->
              updGame_ (creatureAt l . mapped . deckCardEnabled .~ True) })

    , (fire_wall_of_fire,
        defaultCreature
          { onSummoned = \_ ->
              damageCreatures (Effect AbilityDamage) 5 (slotsFor Opponent) })

    , (fire_bargul,
        defaultCreature
          { onSummoned = \l ->
              damageCreatures (Effect AbilityDamage) 4 (delete l allSlots) })

    , (fire_fire_elemental,
        defaultCreature
          { onSummoned = \l ->
              do ~(Just c) <- getCreatureAt l
                 doWizardDamage Opponent c 3
                 damageCreatures (Effect AbilityDamage) 3 (slotsFor Opponent) })

  ]


