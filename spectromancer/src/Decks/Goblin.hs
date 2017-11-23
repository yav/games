module Decks.Goblin where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )

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


  ]




