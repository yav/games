module Decks.Death where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens ( (^.) )
import           Control.Monad (when)

import CardIds
import GameMonad
import Game
import CardTypes
import Deck
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (death_banshee,
        defaultCreature
          { onSummoned = \l ->
              do let l1 = oppositeOf l
                 mb <- getCreatureAt l1
                 case mb of
                   Nothing -> return ()
                   Just c1  ->
                     do let dmg = (c1 ^. deckCardLife + 1) `div` 2
                        damageCreatures (Effect AbilityDamage) dmg [l1] })

    , (death_master_lich,
        defaultCreature
          { onSummoned = \_ ->
              damageCreatures (Effect AbilityDamage) 8 (slotsFor Opponent) })

    , (death_keeper_of_death,
        defaultCreature
          { onDiedOther = \cl dl ->
              do let owner = locWho cl
                 when (owner /= locWho dl) $    -- opponents creature died
                   wizChangePower owner Special 1 })

  ]




