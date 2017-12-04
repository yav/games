module Decks.Goblin where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Monad(when)

import CardIds
import GameMonad
import CardTypes
import EffectAPI

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (goblin's_goblin_raider,
        defaultCreature
          { onSummoned = \l ->
              do Just c <- getCreatureAt l
                 let addFriend = do mb <- randomBlankSlot (locWho l)
                                    case mb of
                                      Nothing -> return ()
                                      Just l' -> summonCreature c l'
                 addFriend
                 addFriend })

    , (goblin's_goblin_hero,
        defaultCreature
          { onSummonedOther = \cl tgt ->
              when (cl == oppositeOf tgt) $
                do mb <- randomBlankSlot (locWho cl)
                   case mb of
                     Nothing -> return () -- oh no, we have nowhere to run!
                     Just l  -> creatureMove cl l })

  , (goblin's_goblin_looter,
      defaultCreature
        { onDiedOther = \cl _ ->
            do el <- randomPower
               wizChangePower (locWho cl) el 1 })
  ]




