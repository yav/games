module Decks.Demonic where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens (view)

import CardIds
import GameMonad
import Game
import Deck
import CardTypes
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (demonic_greater_demon,
        defaultCreature
          { onSummoned = \l ->
              do fp <- withGame (view (player Caster . playerPower Fire))
                 let dmg = min 10 fp
                 ~(Just c) <- getCreatureAt l
                 doWizardDamage Opponent c dmg
                 damageCreatures Effect dmg (slotsFor Opponent) })
  ]




