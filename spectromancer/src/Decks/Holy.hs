module Decks.Holy where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Monad (forM_)

import CardIds
import GameMonad
import Deck
import CardTypes
import EffectAPI

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (holy_paladin,
        defaultCreature
          { onSummoned = \_ ->
              forM_ (slotsFor Caster) $ \s -> creatureChangeLife_ s 4 })

    , (holy_angel,
        defaultCreature
          { onSummoned = \_ -> wizChangePower Caster Special 3 })

    , (holy_archangel,
        defaultCreature
          { onSummoned = \_ ->
              forM_ (slotsFor Caster) $ \s -> creatureChangeLife_ s 100000 })



  ]




