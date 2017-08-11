{-# Language MultiWayIf #-}
module Effects where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad(when)
import Control.Lens((^.),(.~),(%~),(&))

import CardTypes
import CardIds
import Game
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
         addPower p  = p & playerPower %~ Map.unionWith (+) changes

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
     if | g ^. otherPlayer . playerLife <= 0 -> winGame Caster
        | g ^. curPlayer   . playerLife <= 0 -> winGame Opponent
        | otherwise                          -> return ()



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
         Just c | c ^. deckCardLife <= 0 ->
            do updPlayer_ (locWho l) (creatureInSlot (locWhich l) .~ Nothing)
               creatureDie (l,c)
               mapM_ (`creatureDied` l) (slotsFor Caster ++ slotsFor Opponent)
         _  -> return ()





--------------------------------------------------------------------------------
-- Things that can happen to summoned creatures


creatureReact ::
  [(Text, (Location,DeckCard) -> Location -> GameM ())]
                                              {- ^ Special abilities -} ->
  Location {- ^ Creature that is reacting -} ->
  Location {- ^ Location of the arget causing the reaction -} ->
  GameM ()
creatureReact ab = \cl tgtl ->
  do mb <- getCreatureAt cl
     case mb of
       Nothing -> return ()
       Just c ->
         case Map.lookup (deckCardName c) abilities of
           Nothing  -> return ()
           Just act -> act (cl,c) tgtl
  where
  abilities = Map.fromList ab



-- | Actions take by another creature, when a creature is summoned.
-- (e.g., "Dwarven Riflemen")
creatureSummoned ::
  Location {- ^ actor -} ->
  Location {- ^ summoned creature -} ->
  GameM ()
creatureSummoned = creatureReact
    [ (mechanical_dwarven_rifleman,
       \(cl,_) sl -> when (locWho cl /= locWho sl)
                       $ creatureTakeDamage Effect 4 sl
      )
    ]


creatureDied ::
  Location {- ^ actor -} ->
  Location {- ^ where the killed creature was (it is not there anymore) -} ->
  GameM ()
creatureDied = creatureReact
  [ (death_keeper_of_death, \(cl,_) dl ->
       do let owner = locWho cl
          when (owner /= locWho dl) $    -- opponents creature died
            updPlayer_ owner (elementPower Special %~ (+1))
    )
  ]

data DamageSource = Attack | Effect

-- | The creature at this location (if any) should take some damage,
-- if it wishes to.
creatureTakeDamage :: DamageSource -> Int -> Location -> GameM ()
creatureTakeDamage dmg amt l =
  do mb <- getCreatureAt l
     case mb of
       Nothing -> return ()
       Just c ->
        -- XXX: Neighbours could affect how much damage we shoudl actualy
        -- take...
         case Map.lookup (deckCardName c) abilities of
           Just act -> act c
           Nothing  -> doDamage c amt
  where
  abilities = Map.fromList
    [ (water_giant_turtle, \c -> doDamage c (amt - 5))
    , (water_ice_golem, \c -> case dmg of
                                Attack -> doDamage c amt
                                _      -> return ())
    ]

  doDamage c am = doDamage' c am >> return ()

  doDamage' c am =
    do let dmgDone = min (c ^. deckCardLife) (max 0 am)
           c'      = c & deckCardLife %~ subtract dmgDone

       updGame_ (creatureAt l .~ Just c')
       return dmgDone


-- | Effects that happen when a creature dies.
-- The location is where the crature used to be, but it would have
-- already been removed by the time we call this function.
creatureDie :: (Location,DeckCard) -> GameM ()
creatureDie (l,c) =
  case Map.lookup (deckCardName c) abilities of
    Nothing  -> return ()
    Just act -> act
  where
  abilities = Map.fromList
    [ (air_phoenix, updPlayer_ (locWho l) $ \p ->
                      if p ^. elementPower Fire >= 10
                         then let newCard = c & deckCard .~ (c ^. deckCardOrig)
                              in p & creatureInSlot (locWhich l) .~ Just newCard
                         else p)
    ]

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






