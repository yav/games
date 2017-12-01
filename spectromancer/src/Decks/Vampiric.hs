module Decks.Vampiric where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Monad (forM_,when)

import CardIds
import GameMonad
import Game
import Cards
import Deck
import CardTypes
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (vampiric_vampire_elder,
        defaultCreature
          { onSummoned = \l ->
            let fs = newDeckCard Special (getCard other_cards other_initiate)
            in summonLR l fs })

    , (vampiric_magister_of_blood,
        defaultCreature
          { onSummoned = \l ->
            do Just c <- getCreatureAt l
               doWizardDamage Opponent c 16
               forM_ (slotsFor Opponent) $ \loc ->
                 do mb <- getCreatureAt (oppositeOf loc)
                    case mb of
                      Nothing -> return ()
                      Just _  -> damageCreature Effect 16 loc })

    , (vampiric_ghoul,
        defaultCreature
          { onDiedOther = \cl died ->
              when (locWho died == theOtherOne (locWho cl)) $
                creatureChangeAttack cl 1 })


  ]




