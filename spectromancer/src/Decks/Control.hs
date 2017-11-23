module Decks.Control where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Lens ((^.), (%~))

import CardIds
import GameMonad
import Game
import Deck
import CardTypes
import EffectAPI

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [
      (control_goblin_shaman,
        defaultCreature
          { modifyCost = \x -> if isSpell (x ^. deckCard) then 1 else 0 })

    , (control_damping_tower, defaultCreature { modifyCost = \_ -> 1 })

    , (control_ancient_witch,
        defaultCreature
          { onSummoned = \_ ->
              mapM_ (\p -> wizChangePower Opponent p (-2)) allElements })

    , (control_ancient_giant,
        defaultCreature
          { onSummoned = \_ ->
              updGame_ (playerCardNum Opponent %~ subtract 1) })

  ]




