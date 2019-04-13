module Decks.Air where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens (view)
import           Control.Monad (when)

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
      (air_griffin,
        defaultCreature
          { onSummoned = \l ->
              do ~(Just c) <- getCreatureAt l
                 p <- withGame (view (player Caster . playerPower Air))
                 when (p >= 5) (doWizardDamage Opponent c 5) })

    , (air_faerie_sage,
        defaultCreature
          { onSummoned = \_ ->
              do p <- withGame (view (player Caster . playerPower Earth))
                 wizChangeLife Caster (min 10 p) })

    , (air_air_elemental,
        defaultCreature
          { onSummoned = \l -> do ~(Just c) <- getCreatureAt l
                                  doWizardDamage Opponent c 8 })

    , (air_titan,
        defaultCreature
          { onSummoned = \l ->
              damageCreature (Effect AbilityDamage) 15 (oppositeOf l) })


  ]




