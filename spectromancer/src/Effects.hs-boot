module Effects where

import EffectAPI(DamageSource)
import CardTypes(Who,Location)
import Game(Game,DeckCard)
import GameMonad(GameM)

damageCreatures :: DamageSource -> Int -> [Location] -> GameM ()
damageCreature  :: DamageSource -> Int -> Location -> GameM ()
doWizardDamage  :: Who -> DeckCard -> Int -> GameM ()
getAttackPower  :: Game -> (Location, DeckCard) -> Int
creatureKill    :: Location -> GameM ()

