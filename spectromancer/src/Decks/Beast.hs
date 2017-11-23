module Decks.Beast where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )
import           Control.Monad (forM_)

import CardIds
import Cards
import GameMonad
import Game
import Deck
import CardTypes
import EffectAPI

creatures :: Map Text CreatureEffects
creatures = Map.fromList
  [

      (beast_magic_hamster,
        defaultCreature
          { onSummoned = \l ->
              do beastBorn beast_magic_hamster
                 creatureChangeLife_ (leftOf l) 10
                 creatureChangeLife_ (rightOf l) 10 })

    , (beast_scorpion,
        defaultCreature
          { onSummoned = \l ->
              do beastBorn beast_scorpion
                 creatureSkipNextAttack (oppositeOf l) })

    , (beast_wolverine, 
        defaultCreature
          { onSummoned = \_ -> beastBorn beast_wolverine })

    , (beast_energy_beast,
        defaultCreature
          { onSummoned = \_ -> beastBorn beast_energy_beast })

    , (beast_death_falcon,
        defaultCreature
          { onSummoned = \_ -> beastBorn beast_death_falcon })

    , (beast_white_elephant,
        defaultCreature
          { onSummoned = \_ -> beastBorn beast_white_elephant })

    , (beast_basilisk,
        defaultCreature
          { onSummoned = \_ -> beastBorn beast_basilisk })

    , (beast_ancient_dragon,
        defaultCreature
          { onSummoned = \l ->
              do beastBorn beast_ancient_dragon
                 forM_ allElements $ \el -> wizChangePower (locWho l) el 1 })



  ]

beastBorn :: Text -> GameM ()
beastBorn x =
  case Map.lookup x beastAbilityMap of
    Just c  -> updGame_ $ replaceCard Caster x
                        $ newDeckCard Special
                        $ getCard beasts_abilities c
    Nothing -> return () -- Shouldn't happen


-- | Associates beastes with their abilities
beastAbilityMap :: Map Text Text
beastAbilityMap = Map.fromList
    [ (beast_magic_hamster, beast's_natural_healing)
    , (beast_scorpion, beast's_poison)
    , (beast_wolverine, beast's_enrage)
    , (beast_energy_beast, beast's_pump_energy)
    , (beast_death_falcon, beast's_move_falcon)
    , (beast_white_elephant, beast's_trumpet)
    , (beast_basilisk, beast's_gaze)
    , (beast_ancient_dragon, beast's_breathe_fire)
    ]



