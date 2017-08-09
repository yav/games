{-# Language MultiWayIf #-}
module Effects where

import qualified Data.Map as Map
import qualified Data.Text as Text

import CardTypes
import CardIds
import State
import GameMonad
import Deck(Element(..),allElements)


{-
Effects of an action are resolved in this order:
  1) Deal damage to opponent.
  2) Deal damage to opponent's creatures.
  3) Perform additional special effects.
  4) Deal damage to owner's creatures.
  5) Pay for the action (in the case of playing a card)

After an action is resolved we need to do:
  6) Check for end of game, stop if finished, otherwise:
  7) Opponent's creatues die, which may cause further effects.
  8) Caster's creature's die, which may cause further effects.
-}

--------------------------------------------------------------------------------
-- Main phases of a turn

generatePower :: GameM ()
generatePower =
  do ours   <- map snd <$> getCreaturesFor Caster
     theirs <- map snd <$> getCreaturesFor Opponent

     let baseGrowth  = zip allElements (repeat 1)
         ourEffects  = concatMap (creatureModifyPowerGrowth Caster) ours
         theirEffect = concatMap (creatureModifyPowerGrowth Opponent) theirs
         changes     = Map.fromListWith (+)
                          (baseGrowth ++ ourEffects ++ theirEffect)
         addPower p  = p { playerPower = Map.unionWith (+)
                                            changes (playerPower p) }

     updPlayer_ Caster addPower

startOfTurn :: GameM ()
startOfTurn =
  do ours <- getCreaturesFor Caster
     mapM_ creatureStartOfTurn ours

playCard :: DeckCard -> Game -> Game
playCard = undefined

creaturesAttack :: Game -> Game
creaturesAttack = undefined


--------------------------------------------------------------------------------

-- | Is this a finished game?
checkGameWon :: GameM ()
checkGameWon =
  do g <- getGame
     if | playerLife (otherPlayer g) <= 0 -> winGame Caster
        | playerLife (curPlayer   g) <= 0 -> winGame Opponent
        | otherwise                       -> return ()



-- | Activate actions on creatures that just died, and possibly end the game.
checkDeath :: GameM ()
checkDeath =
  do checkGameWon
     mapM_ checkCreature (slotsFor Opponent)
     mapM_ checkCreature (slotsFor Caster)
  where
  checkCreature l =
    do mb <- getCreatureAt l
       case mb of
         Just c | creatureLife (creatureCard c) <= 0 -> creatureDie (l,c)
         _                                           -> return ()





--------------------------------------------------------------------------------
-- Things that can happen to summoned creatures


-- | Actions take by another creature, when a creature is summoned.
-- (e.g., "Dwarven Riflemen")
creatureSummoned ::
  Location {- ^ actor -} ->
  Location {- ^ summoned creature -} ->
  Game -> Game
creatureSummoned = undefined

data DamageSource = Attack | Effect

-- | The creature at this location (if any) should take some damage,
-- if it wishes to.
creatureTakeDamage :: DamageSource -> Int -> Location -> GameM ()
creatureTakeDamage dmg amt l =
  do mb <- getCreatureAt l
     case mb of
       Nothing -> return ()
       Just c ->
         case Map.lookup (deckCardName c) abilities of
           Just act -> act
           Nothing ->
              let cre   = creatureCard c
                  cre'  = cre { creatureLife = creatureLife cre - amt }
                  c'    = c { deckCard = (deckCard c)
                                            { cardEffect = Creature cre' } }
                  upd p = p { playerActive =
                                Map.insert (locWhich l) c' (playerActive p) }
              in updPlayer_ (locWho l) upd


  where
  abilities = Map.fromList
    [
    ]


-- | Effects that happen when a creature dies.
-- The location is where the crature used to be, but it would have
-- already been removed by the time we call this function.
creatureDie :: (Location,DeckCard) -> GameM ()
creatureDie = undefined

-- | The creature at the given location performs its attack, if any.
creatureAttack :: Location -> Game -> Game
creatureAttack = undefined

-- | Compute changes in power growth due to the presence of this creature
-- for either the caster or the opponent.
creatureModifyPowerGrowth :: Who -> DeckCard -> [(Element,Int)]
creatureModifyPowerGrowth w c =
  case Map.lookup name abilities of
    Just (w',es) | w' == w -> es
    _                      -> []
  where
  name = deckCardName c

  abilities = Map.fromList
    [ (fire_priest_of_fire, (Caster, [(Fire,1)]))
    ]


-- | Do something at the beginning of the owner's turn.
creatureStartOfTurn :: (Location,DeckCard) -> GameM ()
creatureStartOfTurn (l,c) =
  case Map.lookup name abilities of
    Nothing -> return ()
    Just act ->
      do addLog (Text.unpack name ++ " start of turn action")
         act
         checkDeath

  where
  name = deckCardName c

  abilities =
    Map.fromList
      [ (fire_goblin_berserker,
           do creatureTakeDamage Effect 2 (leftOf l)
              creatureTakeDamage Effect 2 (rightOf l)
        )

      ]






