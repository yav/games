module Decks.Death where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens ( (^.) )

import CardIds
import GameMonad
import Game
import CardTypes
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
                        damageCreatures Effect dmg [l1] })

    , (death_master_lich,
        defaultCreature
          { onSummoned = \_ -> damageCreatures Effect 8 (slotsFor Opponent) })

  ]




