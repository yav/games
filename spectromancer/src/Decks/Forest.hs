module Decks.Forest where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens ((^.),view)
import           Control.Monad(when)

import CardIds
import GameMonad
import Game
import Deck
import CardTypes
import Cards
import EffectAPI
import {-# SOURCE #-} Effects

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (forest_crazy_squirrel,
        defaultCreature
          { onSummoned = \l ->
              damageCreature (Effect AbilityDamage) 8 (oppositeOf l) })

    , (forest_vindictive_raccoon,
        defaultCreature
          { onSummoned = \l ->
            whenCreature (oppositeOf l) $ \op ->
               do g <- getGame
                  let atk = getAttackPower g (oppositeOf l, op)
                  damageCreature (Effect AbilityDamage) atk (oppositeOf l) })

    , (forest_enraged_beaver,
        defaultCreature
          { onSummoned = \l ->
              do atk <- getRabbitAttack Caster
                 if atk <= 0
                  then return ()
                  else do ~(Just c) <- getCreatureAt l
                          doWizardDamage Opponent c atk
                          damageCreatures (Effect AbilityDamage)
                              atk (slotsFor Opponent) })

    , (forest_bee_queen,
        defaultCreature
          { onSummoned = \l ->
              let bee = newDeckCard Special
                      (getCard other_cards other_bee_soldier)
              in summonLR l bee })



  ]

getRabbitAttack :: Who -> GameM Int
getRabbitAttack w =
  do crs <- findCreature w other_magic_rabbit
     case crs of
      []  -> return 0
      (l,r):_ ->
        case r ^. deckCard . creatureCard . creatureAttack of
          Nothing -> return 0
          Just _  -> do g <- getGame
                        return (getAttackPower g (l,r))


-- | Special start of turn even for the forest cards:
-- if we don't have a rabbit, make one.
maybeSpawnRabbit :: GameM ()
maybeSpawnRabbit =
 do pcls <- withGame (view (player Caster . playerClass))
    when (pcls == forest_cards) $
      do ext_rab <- findCreature Caster other_magic_rabbit
         mbslot <- randomBlankSlot Caster
         case (ext_rab, mbslot) of
           (_, Nothing)    -> return ()
           (_:_, _)        -> return ()
           ([], Just slot) ->
             do let rabbit = newDeckCard Special
                                        (getCard other_cards other_magic_rabbit)
                summonCreature rabbit slot



