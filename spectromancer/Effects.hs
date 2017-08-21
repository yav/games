{-# Language MultiWayIf, OverloadedStrings #-}
module Effects where

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.List(delete,sort)
import Control.Monad(when,unless,forM_)
import Control.Lens((^.),(.~),(%~),(&),at,mapped)
import Util.Random(oneOf)

import CardTypes
import CardIds
import Cards(getCard)
import Game
import GameMonad
import Deck(Element(..),allElements)

import Debug.Trace


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
startOfTurn = mapM_ creatureStartOfTurn (slotsFor Caster)

playCard :: DeckCard -> Maybe Location -> GameM ()
playCard c mbLoc =
  case (c ^. deckCard . cardEffect, mbLoc) of
    (Spell {}, mb) -> do cost <- checkCost
                         castSpell c mb
                         payCost cost
                         checkDeath
    (Creature {}, Just l)
      | locWho l == Caster && locWhich l >= 0 && locWhich l < slotNum ->
        do g <- getGame
           let isEmissary = deckCardName c == death_emissary_of_dorlak
           case g ^. creatureAt l of
             Nothing | isEmissary ->
                          stop "Emissary must be go on top of a creature"
             Just _ | not isEmissary ->
                          stop "Creature must be played on an empty space"
             _ -> do cost <- checkCost
                     let c1 = c & deckCardEnabled .~ False
                     updGame_ (creatureAt l .~ Just c1)
                     creatureSummonEffect (l,c1)
                     payCost cost
                     checkDeath
                     mapM_ (`creatureSummoned` l) allSlots
                     checkDeath
    _ -> stop "Card needs an approprate target"

  where
  stop msg = stopGame (Err msg)

  el = c ^. deckCardElement

  payCost cost = updGame_ ( player Caster . elementPower el %~ subtract cost)

  checkCost = do g <- getGame
                 let base = c ^. deckCard . cardCost
                     cost = base + extraCost g
                     have = g ^. player Caster . elementPower el
                 when (cost > have) (stop "Card needs more power")
                 return cost

  extraCost g = sum
              $ map (creatureModifyCost c . snd)
              $ inhabitedSlots g
              $ slotsFor Opponent



-- | Do the attack phase of a turn.
creaturesAttack :: GameM ()
creaturesAttack = mapM_ performCreatureAttack (slotsFor Caster)

--------------------------------------------------------------------------------

-- | Is this a finished game?
checkGameWon :: GameM ()
checkGameWon =
  do g <- getGame
     if | g ^. otherPlayer . playerLife <= 0 -> stopGame (GameWonBy Caster)
        | g ^. curPlayer   . playerLife <= 0 -> stopGame (GameWonBy Opponent)
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
               mapM_ (`creatureDied` l) allSlots
         _  -> return ()





--------------------------------------------------------------------------------

castSpell :: DeckCard -> Maybe Location -> GameM ()
castSpell c mbTgt =
  case Map.lookup (deckCardName c) spells of
    Just act -> act
    Nothing  -> addLog ("Not yet implemented: " ++ show (deckCardName c))
  where
  target = case mbTgt of
             Nothing -> stopError "Spell needs target"
             Just t  -> return t

  casterTarget =
    do tgt <- target
       case locWho tgt of
         Caster -> return tgt
         _      -> stopError "This spell only affects the caster"

  opponentTarget =
    do tgt <- target
       case locWho tgt of
         Opponent -> return tgt
         _        -> stopError "This spell only affects the opponent"


  damageSpell k =
    do g <- getGame
       let cs      = map snd (inhabitedSlots g (slotsFor Caster))
           scaling = product (map creatureModifySpellDamageMul cs)
           add     = sum (map creatureModifySpellDamageAdd cs)
       traceShow (scaling,add) $ k (\d -> ceiling (d * scaling) + add)

  spells = Map.fromList
    [ (fire_flame_wave, damageSpell $ \dmg ->
                            damageCreatures Effect (dmg 9) (slotsFor Opponent))
    , (fire_inferno, damageSpell $ \dmg ->
                     do t <- opponentTarget
                        damageCreatures Effect (dmg 18) [t]
                        damageCreatures Effect (dmg 10)
                                            (delete t (slotsFor Opponent))
      )
    , (fire_armageddon, damageSpell $ \dmg ->
                        do fp <- withGame (player Caster . elementPower Fire)
                           let d = dmg (8 + fromIntegral fp)
                           doWizardDamage Opponent c d
                           damageCreatures Effect d allSlots)

    , (water_meditation,
        forM_ [ Fire, Air, Earth ] $ \el ->
          updGame_ (player Caster . elementPower el %~ (+1))
      )

    , (water_acidic_rain, damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 15) allSlots
           forM_ allElements $ \el ->
             updGame_ (player Opponent . elementPower el %~ subtract 1))

    , (air_call_to_thunder, damageSpell $ \dmg ->
          do tgt <- opponentTarget
             doWizardDamage Opponent c (dmg 6)
             damageCreatures Effect (dmg 6) [tgt])

    , (air_lightning_bolt, damageSpell $ \dmg ->
         do p <- withGame (player Caster . elementPower Air)
            doWizardDamage Opponent c (dmg (fromIntegral p + 5)))

    , (air_chain_lightning, damageSpell $ \dmg ->
        do doWizardDamage Opponent c (dmg 9)
           damageCreatures Effect (dmg 9) (slotsFor Opponent))

    , (air_tornado, do tgt <- opponentTarget
                       updGame_ (creatureAt tgt .~ Nothing))

    , (earth_natures_ritual, do tgt <- casterTarget
                                healCreature tgt 8
                                healOwner 8)
    , (earth_rejuvenation,
        do p <- withGame (player Caster . elementPower Earth)
           healOwner (2 * p))
    , (earth_stone_rain,  damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 25) allSlots)

    , (earth_natures_fury, damageSpell $ \dmg ->
        do g <- getGame
           let d = sum . take 2 . reverse . sort $ atks
               atks = (map (getAttackPower g) (inhabitedSlots g (slotsFor Caster)))
           when (d > 0) $ doWizardDamage Opponent c (dmg (fromIntegral d)))

    , (death_cursed_fog, damageSpell $ \dmg ->
         do doWizardDamage Opponent c (dmg 3)
            damageCreatures Effect (dmg 12) allSlots)

    , (death_dark_ritual, damageSpell $ \dmg ->
          do damageCreatures Effect (dmg 3) (slotsFor Opponent)
             forM_ (slotsFor Caster) $ \s -> healCreature s 3)

    , (death_blood_ritual, damageSpell $ \dmg ->
        do tgt <- casterTarget
           mb  <- withGame (creatureAt tgt)
           case mb of
             Nothing -> stopError "We need a target creature"
             Just cr  ->
               do let d = min 32 (fromIntegral (cr ^. deckCardLife))
                  updGame_ (creatureAt tgt .~ Nothing)
                  damageCreatures Effect (dmg d) (slotsFor Opponent))

    , (death_drain_souls,
        do deaths <- sum <$> mapM creatureKill allSlots
           healOwner (2 * deaths)
           let newCard = newDeckCard Special
                                      (getCard other_cards other_rage_of_souls)
           updGame_ (replaceCard Caster death_drain_souls newCard)
      )

    , (other_rage_of_souls, damageSpell $ \dmg ->
        do p <- withGame (player Caster . elementPower Special)
           let opp = slotsFor Opponent
           damageCreatures Effect (dmg (fromIntegral p + 9)) opp
           g <- getGame
           healOwner $
              sum [ 2 | (_,d) <- inhabitedSlots g opp, d ^. deckCardLife <= 0 ])
    , (holy_divine_justice, damageSpell $ \dmg ->
        do tgt <- casterTarget
           healCreature tgt 12
           damageCreatures Effect (dmg 12) (delete tgt allSlots))
    , (holy_divine_intervention, 
        do forM_ [ Fire, Air, Earth, Water ] $ \el ->
              updGame_ (player Caster . elementPower el %~ (+2))
           healOwner 10)
    , (holy_wrath_of_god, damageSpell $ \dmg ->
        do let opp = slotsFor Opponent
           damageCreatures Effect (dmg 12) opp
           g <- getGame
           let srv = sum $ 
                 [ 1 | (_,d) <- inhabitedSlots g opp, d ^. deckCardLife > 0 ]
           updGame_ (player Caster . elementPower Special %~ (+srv))
      )
    ]

--------------------------------------------------------------------------------
-- Things that can happen to summoned creatures

creatureSummonEffect :: (Location,DeckCard) -> GameM ()
creatureSummonEffect (l,c) =
  case Map.lookup (deckCardName c) abilities of
    Nothing  -> return ()
    Just act -> act
  where
  abilities = Map.fromList
    [ (fire_fire_drake,
          updGame_ (creatureAt l . mapped . deckCardEnabled .~ True))

    , (fire_wall_of_fire, damageCreatures Effect 5 (slotsFor Opponent))
    , (fire_bargul, damageCreatures Effect 4 (delete l allSlots))
    , (fire_fire_elemental,
          do doWizardDamage Opponent c 3
             damageCreatures Effect 3 (slotsFor Opponent)
      )

    , (water_merfolk_apostate,
          updGame_ (player Caster . elementPower Fire %~ (+2)))
    , (water_water_elemental, healOwner 10)

    , (air_griffin,
        do p <- withGame (player Caster . elementPower Air)
           when (p >= 5) (doWizardDamage Opponent c 5))
    , (air_faerie_sage,
        do p <- withGame (player Caster . elementPower Earth)
           healOwner (min 10 p))
    , (air_air_elemental, doWizardDamage Opponent c 8)
    , (air_titan, creatureTakeDamage Effect 15 (oppositeOf l))

    , (earth_giant_spider,
        do let fs = newDeckCard Earth (getCard other_cards other_forest_spider)
               place l' = when (onBoard l') $
                            do mb <- withGame (creatureAt l')
                               case mb of
                                 Nothing -> updGame_ (creatureAt l' .~ Just fs)
                                 Just _  -> return ()
           place (leftOf l)
           place (rightOf l))

    , (death_banshee,
        do let l1 = oppositeOf l
           mb <- withGame (creatureAt l1)
           case mb of
             Nothing -> return ()
             Just c1  ->
               do let dmg = (c1 ^. deckCardLife + 1) `div` 2
                  damageCreatures Effect dmg [l1])

    , (death_master_lich, damageCreatures Effect 8 (slotsFor Opponent))
    , (holy_paladin, forM_ (slotsFor Caster) $ \s -> healCreature s 4)
    , (holy_angel, updGame_ (player Caster . elementPower Special %~ (+3)))
    , (holy_archangel, forM_ (slotsFor Caster) $ \s -> healCreature s 100000)
    ]


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
    [ (water_merfolk_overlord, \(cl,_) tgt ->
         when (isNeighbor cl tgt) $
           updGame_ (creatureAt tgt . mapped . deckCardEnabled .~ True))

    , (mechanical_dwarven_rifleman, \(cl,_) sl ->
        when (locWho cl /= locWho sl)
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
  do addLog ("creature take damage: " ++ show l)
     g <- getGame
     mb <- getCreatureAt l
     case mb of
       Nothing -> return ()
       Just c ->
        -- XXX: Neighbours could affect how much damage we shoudl actualy
        -- take...
         case Map.lookup (deckCardName c) abilities of
           Just act -> act c
           Nothing  -> doDamage c (damageMods g amt)
  where
  abilities = Map.fromList
    [ (water_giant_turtle, \c -> doDamage c (amt - 5))
    , (water_ice_golem, \c -> case dmg of
                                Attack -> doDamage c amt
                                _      -> return ())
    ]
  modAbilities = Map.fromList
    [ (holy_holy_guard, \me oth ->
        if isNeighbor me oth then (subtract 2) else id)
    ]

  creatureOr g lc m b =
    case cat of
      Nothing -> b
      Just cr ->
        case Map.lookup (deckCardName cr) m of
          Nothing -> b
          Just ent -> ent
    where
      cat = g ^. creatureAt lc

  damageMods g =
    foldr (.) id (abils)
    where
      nullAbil _ _ = id
      otherSlots = delete l allSlots
      abils = map (\lc -> (creatureOr g lc modAbilities nullAbil) lc l) otherSlots

  doDamage c am = doDamage' c am >> return ()

  doDamage' c am =
    do let dmgDone = min (c ^. deckCardLife) (max 0 am)
           c'      = c & deckCardLife %~ subtract dmgDone
       addLog ("really doing damage to " ++ show (deckCardName c)
                                         ++ show dmgDone)
       updGame_ (creatureAt l .~ Just c')
       return dmgDone


creatureKill :: Location -> GameM Int
creatureKill l =
  do mb <- withGame (creatureAt l)
     case mb of
       Nothing -> return 0
       Just c  -> do updGame_ (creatureAt l .~ Nothing)
                     creatureDie (l,c)
                     return 1

-- | Effects that happen when a creature dies.
-- The location is where the crature used to be, but it would have
-- already been removed by the time we call this function.
-- Note that the creature's life might not be 0, if it got killed
-- through some odd way (e.g., drain souls)
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
      , (holy_monk, updGame_ (player (locWho l) . elementPower Special %~ (+2)))
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
           do addLog ("Creature attacking: " ++ show l)
              let p = getAttackPower g (l,c)
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
    , (death_master_lich, \c p ->
        do let opp = oppositeOf l
           mb <- withGame (creatureAt opp)
           case mb of
             Nothing ->
               do damaged <- doWizardDamage' otherWizard c p
                  when damaged $
                    updGame_ (player (locWho l) . elementPower Special %~ (+2))
             _ -> creatureTakeDamage Attack p opp)
    ]

  damageEveryone c p =
    do doWizardDamage otherWizard c p
       damageCreatures Attack p (slotsFor otherWizard)

-- | Damage the creatures in the given location.
damageCreatures :: DamageSource -> Int -> [Location] -> GameM ()
damageCreatures ty amt ls = mapM_ (creatureTakeDamage ty amt) ls

healOwner :: Int -> GameM ()
healOwner n = updGame_ (player Caster . playerLife %~ (+n))

doWizardDamage :: Who      {- ^ Damage this wizzard -} ->
                  DeckCard {- ^ This is the attacker (creature or spell) -} ->
                  Int      {- ^ Amount of damage we are trying to do -} ->
                  GameM ()  -- ^ Did we actually do any damage
doWizardDamage w d n = doWizardDamage' w d n >> return ()

-- | Deal some damage to one of the two players.
doWizardDamage' :: Who      {- ^ Damage this wizzard -} ->
                  DeckCard {- ^ This is the attacker (creature or spell) -} ->
                  Int      {- ^ Amount of damage we are trying to do -} ->
                  GameM Bool -- ^ Did we actually do any damage
doWizardDamage' who dc amt =
  do -- XXX: The stuff below happens only if White Elephant is not around.
     checkGoblinSaboteur
     amt1 <- checkIceGuard
     updGame_ (player who . playerLife %~ subtract amt1)
     return (amt1 > 0)

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



-- | How is the cost of a played card affected by the active creatures.
creatureModifyCost ::
  DeckCard            {- ^ Card that is being played -} ->
  DeckCard            {- ^ Opponent's creature affecting the cost -} ->
  Int                 {- ^ Change to the cost -}
creatureModifyCost c dc = Map.findWithDefault 0 (deckCardName dc) $
  Map.fromList
    [ (control_goblin_shaman, if isSpell (c ^. deckCard) then 1 else 0)
    , (control_damping_tower, 1)
    ]


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
    [ (fire_priest_of_fire,   (Caster, [(Fire,1)]))
    , (fire_fire_elemental,   (Caster, [(Fire,1)]))

    , (water_merfolk_elder,   (Caster,   [(Air,1)]))
    , (water_water_elemental, (Caster,   [(Water,1)]))
    , (water_mind_master,     (Caster,   [ (e, 1) | e <- allElements ]))
    , (water_astral_guard,    (Opponent, [ (e,-1) | e <- allElements ]))

    , (air_air_elemental,     (Caster, [(Air,1)]))
    , (earth_earth_elemental, (Caster, [(Earth,1)]))
    , (earth_elf_hermit,      (Caster, [(Earth,2)]))
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

creatureModifySpellDamageAdd ::
  DeckCard {- ^ Summoned crature modifying -} ->
  Int
creatureModifySpellDamageAdd c =
  Map.findWithDefault 0 (deckCardName c) abilities
  where
  abilities = Map.fromList
    [ (air_faerie_apprentice, 1)
    ]

creatureModifySpellDamageMul ::
  DeckCard {- ^ Summoned crature modifying -} ->
  Rational
creatureModifySpellDamageMul c =
  Map.findWithDefault 1 (deckCardName c) abilities
  where
  abilities = Map.fromList
    [ (fire_dragon, 3/2)
    ]



-- | Do something at the beginning of the owner's turn.
creatureStartOfTurn :: Location -> GameM ()
creatureStartOfTurn l =
  do mb <- withGame (creatureAt l)
     case mb of
       Nothing -> return ()
       Just c ->
         do let name = deckCardName c
            updGame_ (creatureAt l . mapped . deckCardEnabled .~ True)
            case Map.lookup name abilities of
              Nothing -> return ()
              Just act ->
                do addLog (Text.unpack name ++ " start of turn action")
                   act
                   checkDeath
  where
  card = do mb <- withGame (creatureAt l)
            case mb of
              Nothing -> stopError "[bug] Creature disappeared!"
              Just c  -> return c

  abilities =
    Map.fromList
      [ (fire_goblin_berserker,
           do addLog "Goblin bersker start of turn"
              creatureTakeDamage Effect 2 (leftOf l)
              creatureTakeDamage Effect 2 (rightOf l)
        )

      , (water_sea_sprite, card >>= \c -> doWizardDamage Caster c 2)

      , (air_wall_of_lightning, card >>= \c -> doWizardDamage Opponent c 4)

      , (earth_elven_healer, healOwner 3)
      , (earth_troll, healCreature l 4)

      , (earth_master_healer,
            do healOwner 3
               forM_ (slotsFor Caster) $ \s -> healCreature s 3)

      , (earth_hydra, healCreature l 4)
      ]


healCreature :: Location -> Int -> GameM ()
healCreature l n = do addLog ("Healing " ++ show l)
                      updGame_ (creatureAt l . mapped %~ upd)
  where
  upd d = d & deckCardLife %~ increase
    where
    maxLife    = d ^. deckCardOrig . cardEffect . creatureCard . creatureLife
    increase c = min (c + n) maxLife






