module Decks.Illusion where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens ( (^.) )
import           Control.Monad (forM_)

import CardIds
import GameMonad
import Game
import CardTypes
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
       (illusion_spectral_assassin,
        defaultCreature
          { onSummoned = \l -> do ~(Just c) <- getCreatureAt l
                                  doWizardDamage Opponent c 12 })

    , (illusion_spectral_mage,
        defaultCreature
          { onSummoned = \_ ->
              do g <- getGame
                 let opp = inhabitedSlots g (slotsFor Opponent)
                 forM_ opp $ \(ol,oc) ->
                    damageCreature Effect (oc ^. deckCard . cardCost) ol })

    , (illusion_hypnotist,
        defaultCreature
          { onSummoned = \l ->
              do ~(Just c) <- getCreatureAt l
                 doWizardDamage Opponent c 5
                 damageCreatures Effect 5 (slotsFor Opponent) })

  ]




