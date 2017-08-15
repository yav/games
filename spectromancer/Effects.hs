{-# Language MultiWayIf #-}
module Effects where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad(when,unless)
import Control.Lens((^.),(.~),(%~),(&),at)
import Util.Random(oneOf)

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

-- | Do this at the start of each turn.
startOfTurn :: GameM ()
startOfTurn =
  do ours <- getCreaturesFor Caster
     mapM_ creatureStartOfTurn ours

playCard :: DeckCard -> Game -> Game
playCard = undefined

-- | Do the attack phase of a turn.
creaturesAttack :: GameM ()
creaturesAttack = mapM_ performCreatureAttack (slotsFor Caster)

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
performCreatureAttack :: Location -> GameM ()
performCreatureAttack l =
  do g <- getGame
     case g ^. creatureAt l of
       Nothing -> return ()
       Just c
         | isWall c || not (c ^. deckCardEnabled)    -> return ()
         | otherwise  ->
           do let p = getAttackPower g (l,c)
              case Map.lookup (deckCardName c) abilities of
                Just act -> act c p
                Nothing  ->
                  do let loc = oppositeOf l
                     case g ^. creatureAt loc of
                       Nothing -> doWizardDamage otherWizard c p
                       Just _  -> creatureTakeDamage Attack p loc
              checkDeath
  where
  otherWizard = theOtherOne (locWho l)

  abilities = Map.fromList
    [ (earth_hydra, damageEveryone)
    , (earth_forest_sprite, damageEveryone)
    , (air_lightning_cloud, damageEveryone)
    ]

  damageEveryone c p =
    do doWizardDamage otherWizard c p
       mapM_ (creatureTakeDamage Attack p) (slotsFor otherWizard)


-- | Deal some damage to one of the two players.
doWizardDamage :: Who      {- ^ Damage this wizzard -} ->
                  DeckCard {- ^ This is the attacker (creature or spell) -} ->
                  Int      {- ^ Amount of damage we are trying to do -} ->
                  GameM ()
doWizardDamage who dc amt =
  do -- XXX: The stuff below happens only if White Elephant is not around.
     checkGoblinSaboteur
     amt1 <- checkIceGuard
     updGame_ (player who . playerLife %~ subtract amt1)

  where
  checkGoblinSaboteur =
    when (deckCardName dc == goblin's_goblin_saboteur) $
    do g <- getGame
       let p   = g ^. player who
           els = Map.keys (Map.filter (not . null) (p ^. playerDeck))
       unless (null els) $
         do el <- random (oneOf els)
            updGame_ (player who . playerDeck . at el %~ fmap tail)


  checkIceGuard =
    do g <- getGame
       let halfRoundUp _ x = div (x + 1) 2
       return $ foldr halfRoundUp amt
              $ filter ((water_ice_guard ==) . deckCardName)
              $ map snd
              $ inhabitedSlots g (slotsFor who)




-- | Compute the current attack power for the given creature.
getAttackPower :: Game -> (Location, DeckCard) -> Int
getAttackPower g (l,c) = max 0 (base + change + c ^. deckCardAttackChange)

  where
  ourCreatures   = inhabitedSlots g (slotsFor Caster)
  theirCreatures = inhabitedSlots g (slotsFor Opponent)

  change = sum $ map (creatureModifyAttack (l,c))
               $ ourCreatures ++ theirCreatures

  name  = deckCardName c
  owner = locWho l

  base = case c ^. deckCard . cardEffect . creatureCard . creatureAttack of
           Just a -> a
           Nothing
             | name == fire_fire_elemental    -> elemental Fire
             | name == air_air_elemental      -> elemental Air
             | name == earth_earth_elemental  -> elemental Earth
             | name == water_water_elemental  -> elemental Water
             | name == golem_golem_instructor ->
                  length $ case owner of
                             Caster   -> ourCreatures
                             Opponent -> theirCreatures

             | otherwise -> error "[bug]: Missing base attack"

  elemental s = g ^. player owner . elementPower s




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

-- | Compute changes to the attack value of a speicif creature.
creatureModifyAttack :: (Location,DeckCard) {- ^ Attack of this -} ->
                        (Location,DeckCard) {- ^ Modifier of attack -} -> Int

creatureModifyAttack (l,d) (l1,c)
  | isWall d = 0
  | name == fire_orc_chieftain && isNeighbor l l1 = 2
  | name == fire_minotaur_commander && ours = 1
  | name == golem_golem_instructor && ours && deckCardName d == other_golem = 2
  | otherwise = 0
  where
  name = deckCardName c
  ours = sameSide l l1

isWall :: DeckCard -> Bool
isWall d = deckCardName d `elem` walls
  where walls = [ fire_wall_of_fire
                , illusion_wall_of_reflection
                , air_wall_of_lightning
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






