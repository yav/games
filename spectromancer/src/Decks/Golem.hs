module Decks.Golem where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens(view)
import           Control.Monad(forM_)

import CardIds
import GameMonad
import Game
import CardTypes
import Deck
import Cards
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (golem_golem_guide,
        defaultCreature
          { onSummoned = \l ->
            do (golem_loc, _) <- getGolem
               Just c <- getCreatureAt l
               doSomething l
               creatureMove golem_loc l
               summonCreature c golem_loc })

    , (golem_guardian_statue,
        defaultCreature
          { onSummoned = \l -> creatureMod l Immune UntilNextAttack })

    , (golem_dark_sculptor,
        defaultCreature
          { onSummoned = \_ ->
            do g <- getGame
               let dmg = length (inhabitedSlots g allSlots)
               damageCreatures Effect dmg (slotsFor Opponent) })
  ]

-- | Find out golem
getGolem :: GameM (Location,DeckCard)
getGolem = do xs <- findCreature Caster other_golem
              case xs of
                [x] -> return x
                _ -> error "Missing golem; or multiple ones."


-- | Special start of turn even for the forest cards:
-- if we don't have a rabbit, make one.
maybeSpawnGolem :: Who -> GameM (Maybe Location)
maybeSpawnGolem who =
 do pcls <- withGame (view (player who . playerClass))
    if pcls == golem_cards
      then
        do mbSlot <- randomBlankSlot who
           forM_ mbSlot $ \slot ->
             do let golem = newDeckCard Special
                          (getCard other_cards other_golem)
                summonCreature golem slot
           return mbSlot
      else return Nothing






