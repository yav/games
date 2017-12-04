module Decks.Spirit where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Monad (forM_,when)

import CardIds
import GameMonad
import Deck
import CardTypes
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (spirit_crusader,
        defaultCreature
          { onSummoned = \_ ->
            forM_ (slotsFor Caster) $ \s -> creatureChangeLife_ s 2 })

    , (spirit_angel, defaultCreature
          { onSummoned = \_ ->  wizChangePower Caster Special 3 })

    , (spirit_angel_of_war,
        defaultCreature
          { onSummoned = \_ ->
            do damageCreatures Effect 8 (slotsFor Opponent)
               forM_ (slotsFor Caster) $ \s -> creatureChangeLife_ s 8 })

    , (spirit_templar,
        defaultCreature
          { onSummonedOther = \cl sl ->
              when (isNeighbor cl sl) $
                do Just c <- getCreatureAt cl
                   doWizardDamage (theOtherOne (locWho cl)) c 4 })

  , (spirit_holy_avenger,
        defaultCreature
          { onDiedOther = \cl died ->
              when (isNeighbor cl died) (creatureChangeAttack cl 2) })


  ]




