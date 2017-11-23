{-# Language OverloadedStrings #-}
module Effects
  ( creatureModifyPowerGrowth
  , creatureStartOfTurn
  , playCard
  , creaturePerformAttack
  , creatureEndOfTurn

  , damageCreatures
  , damageCreature
  , doWizardDamage
  , getAttackPower
  ) where

import qualified Data.Map as Map
import           Data.List(delete,sort,maximumBy)
import           Data.Function(on)
import           Data.Foldable(for_)
import           Data.Maybe(fromMaybe,catMaybes)
import Control.Monad(when,unless,forM_,replicateM_)
import Control.Lens((^.),(.~),(%~),(&),at,mapped,view)
import Util.Random(oneOf, randInRange)

import CardTypes
import CardIds
import Cards(getCard)
import Game
import GameMonad
import Deck(Element(..),allElements)
import EffectAPI

import qualified Decks.Fire
import qualified Decks.Water
import qualified Decks.Air
import qualified Decks.Earth
import qualified Decks.Death
import qualified Decks.Holy
import qualified Decks.Mechanical
import qualified Decks.Illusion
import qualified Decks.Beast
import qualified Decks.Goblin
import qualified Decks.Forest
import qualified Decks.Demonic
import qualified Decks.Golem
import qualified Decks.Spirit
import qualified Decks.Vampiric
import qualified Decks.Control



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



-- | Compute changes in power growth due to the presence of this creature
-- for either the caster or the opponent.
creatureModifyPowerGrowth :: Who -> DeckCard -> [(Element,Int)]
creatureModifyPowerGrowth w c =
  case Map.lookup (deckCardName c) abilities of
    Just (w',es) | w' == w -> es
    _                      -> []
  where
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

    , (mechanical_dwarven_craftsman, (Caster, [(Special, 1)]))

    , (demonic_demon_quartermaster, (Caster, [(Special, 1)]))
    ]




-- | Special abilities that activate at the start of a player's turn.
creatureStartOfTurn :: Location {- ^ Location ot check for effect -} ->
                       GameM ()
creatureStartOfTurn l =
  whenCreature l $ \c ->
  do updGame_ (creatureAt l . mapped . deckCardEnabled .~ True)-- now can attack
     let name = deckCardName c
     case Map.lookup name abilities of
       Nothing -> return ()
       Just act ->
         do doSomething l
            act
            checkDeath
  where
  card = do mb <- getCreatureAt l
            case mb of
              Nothing -> stopError "[bug] Creature disappeared!"
              Just c  -> return c

  abilities =
    Map.fromList
      [ (fire_goblin_berserker,
           do damageCreature Effect 2 (leftOf l)
              damageCreature Effect 2 (rightOf l)
        )

      , (water_sea_sprite, card >>= \c -> doWizardDamage Caster c 2)

      , (air_wall_of_lightning, card >>= \c -> doWizardDamage Opponent c 4)


      , (earth_elven_healer, wizChangeLife Caster 3)
      , (earth_troll, creatureChangeLife_ l 4)
      , (earth_master_healer,
            do wizChangeLife Caster 3
               forM_ (slotsFor Caster) $ \s -> creatureChangeLife_ s 3)

      , (earth_hydra, creatureChangeLife_ l 4)


      , (mechanical_ornithopter, damageCreatures Effect 2 (slotsFor Opponent))
      , (mechanical_cannon, 
          do g <- getGame
             let oppCr = inhabitedSlots g (slotsFor Opponent)
             case oppCr of
              [] -> return ()
              _  -> do
                let compareLife = (compare `on` (\(_,c1) -> c1 ^. deckCardLife))
                    (lh,_) = (maximumBy compareLife oppCr)
                damageCreatures Effect 8 [lh]
        )


      , (illusion_oracle,
          do p <- withGame (view (player Caster . playerPower Special))
             c <- card
             doWizardDamage Opponent c p)

      , (goblin's_ratmaster,
            do damageCreatures Effect 6 (slotsFor Opponent)
               el <- randomPower
               wizChangePower Caster el (-3))


      , (chaos_insanian_peacekeeper,
          do amt <- random (randInRange 1 6) 
             wizChangeLife Caster amt)
      , (chaos_insanian_berserker,
          do amt <- random (randInRange 1 6)
             c <- card
             doWizardDamage Opponent c amt)
      , (chaos_insanian_shaman,
          do elt <- random $ oneOf allElements
             wizChangePower Opponent elt (-2))
      , (chaos_insanian_lord,
          do elt <- random $ oneOf allElements
             wizChangePower Caster elt 2)
      , (chaos_insanian_catapult,
          do mbtgt <- randomCreature Opponent
             case mbtgt of
               Nothing  -> return ()
               Just tgt -> damageCreature Effect 10 tgt)


      , (other_magic_rabbit, creatureChangeAttack l 1)

      -- Vampiric
      , (vampiric_devoted_servant, creatureChangeAttack l 1)
      ]















-- | Implementations of spell cards.
castSpell :: DeckCard -> Maybe Location -> GameM ()
castSpell c mbTgt =
  case Map.lookup (deckCardName c) spells of
    Just act -> act
    Nothing  -> addLog (Say ("Not yet implemented: " ++ show (deckCardName c)))
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

  creature tgt =
    do occupant <- getCreatureAt tgt
       case occupant of
          Nothing -> stopError "Target must contain a creature"
          Just _  -> return ()
       return tgt

  damageSpell k =
    do g <- getGame
       let cs      = map snd (inhabitedSlots g (slotsFor Caster))
           es      = map snd (inhabitedSlots g (slotsFor Opponent))
           scaling = product (map creatureModifySpellDamageMul cs)
           add     = sum (map (creatureModifySpellDamageAdd Caster) cs)
                     + sum (map (creatureModifySpellDamageAdd Opponent) es)
       k (\d -> ceiling (d * scaling) + add)

  furySpell dmg w =
    do g <- getGame
       let d = sum . take 2 . reverse . sort $ atks
           atks = (map (getAttackPower g) (inhabitedSlots g (slotsFor w)))
       when (d > 0) $ doWizardDamage Opponent c (dmg (fromIntegral d))


  findBeast x =
    do xs <- findCreature Caster x
       case xs of
         []  -> stopError "This beast is not present"
         [r] -> return r
         _   -> stopError "[bug] Multiple beasts"

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
                        do fp <- withGame
                                   (view (player Caster . playerPower Fire))
                           let d = dmg (8 + fromIntegral fp)
                           doWizardDamage Opponent c d
                           damageCreatures Effect d allSlots)

    , (water_meditation,
        forM_ [ Fire, Air, Earth ] $ \el -> wizChangePower Caster el 1)

    , (water_acidic_rain, damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 15) allSlots
           forM_ allElements $ \el -> wizChangePower Opponent el (-1))

    , (air_call_to_thunder, damageSpell $ \dmg ->
          do tgt <- opponentTarget
             doWizardDamage Opponent c (dmg 6)
             damageCreature Effect (dmg 6) tgt)

    , (air_lightning_bolt, damageSpell $ \dmg ->
         do p <- withGame (view (player Caster . playerPower Air))
            doWizardDamage Opponent c (dmg (fromIntegral p + 5)))

    , (air_chain_lightning, damageSpell $ \dmg ->
        do doWizardDamage Opponent c (dmg 9)
           damageCreatures Effect (dmg 9) (slotsFor Opponent))

    , (air_tornado, do tgt <- opponentTarget
                       creatureDestroy tgt)

    , (earth_natures_ritual, do tgt <- casterTarget
                                creatureChangeLife_ tgt 8
                                wizChangeLife Caster 8)
    , (earth_rejuvenation,
        do p <- withGame (view (player Caster . playerPower Earth))
           wizChangeLife Caster (2 * p))
    , (earth_stone_rain,  damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 25) allSlots)

    , (earth_natures_fury, damageSpell $ \dmg ->
        furySpell dmg Caster)

    , (death_cursed_fog, damageSpell $ \dmg ->
         do doWizardDamage Opponent c (dmg 3)
            damageCreatures Effect (dmg 12) allSlots)

    , (death_dark_ritual, damageSpell $ \dmg ->
          do damageCreatures Effect (dmg 3) (slotsFor Opponent)
             forM_ (slotsFor Caster) $ \s -> creatureChangeLife_ s 3)

    , (death_blood_ritual, damageSpell $ \dmg ->
        do tgt <- casterTarget
           mb  <- getCreatureAt tgt
           case mb of
             Nothing -> stopError "We need a target creature"
             Just cr  ->
               do let d = min 32 (fromIntegral (cr ^. deckCardLife))
                  creatureDestroy tgt
                  damageCreatures Effect (dmg d) (slotsFor Opponent))

    , (death_drain_souls,
        do deaths <- do g <- getGame
                        return (length (inhabitedSlots g allSlots))
           mapM_ creatureKill (slotsFor Opponent)
           mapM_ creatureKill (slotsFor Caster)
           wizChangeLife Caster (2 * deaths)
           let newCard = newDeckCard Special
                                      (getCard other_cards other_rage_of_souls)
           updGame_ (replaceCard Caster death_drain_souls newCard)
      )

    , (other_rage_of_souls, damageSpell $ \dmg ->
        do p <- withGame (view (player Caster . playerPower Special))
           let opp = slotsFor Opponent
           damageCreatures Effect (dmg (fromIntegral p + 9)) opp
           (_,deaths) <- countLiving opp
           wizChangeLife Caster (2 * deaths))

    -- Holy spells
    , (holy_divine_justice, damageSpell $ \dmg ->
        do tgt <- casterTarget
           creatureChangeLife_ tgt 12
           damageCreatures Effect (dmg 12) (delete tgt allSlots))
    , (holy_divine_intervention,
        do forM_ [ Fire, Air, Earth, Water ] $ \el -> wizChangePower Caster el 2
           wizChangeLife Caster 10)
    , (holy_wrath_of_god, damageSpell $ \dmg ->
        do let opp = slotsFor Opponent
           damageCreatures Effect (dmg 12) opp
           (alive,_) <- countLiving opp
           wizChangePower Caster Special alive
      )

    -- Mechanical spells
    , (mechanical_overtime, wizChangePower Caster Special 1)
    , (mechanical_cannonade, damageSpell $ \dmg ->
        damageCreatures Effect (dmg 19) (slotsFor Opponent))


    -- Illusion
    , (illusion_madness, damageSpell $ \dmg ->
      do g <- getGame
         let opp  = inhabitedSlots g (slotsFor Opponent)
         forM_ opp $ \(cl, cc) ->
           do let damage = dmg . fromIntegral . (getAttackPower g) $ (cl, cc)
              damageCreature Effect damage cl)
    , (illusion_hypnosis, damageSpell $ \dmg ->
        furySpell dmg Opponent)


    -- Beast Abilities --------------------------------------
    , (beast's_trumpet, wizChangePower Caster Special 1)
    , (beast's_gaze, damageSpell $ \dmg ->
                        damageCreature Effect (dmg 6) =<< creature =<< target)
    , (beast's_pump_energy,
        forM_ [Fire,Water,Air,Earth] $ \el -> wizChangePower Caster el 1)
    , (beast's_move_falcon, damageSpell $ \dmg ->
        do (l,_)  <- findBeast beast_death_falcon
           creatureMove l =<< casterTarget
           damageCreatures Effect (dmg 4) (slotsFor Opponent))

    , (beast's_poison, damageSpell $ \dmg ->
        damageCreature Effect (dmg 14) =<< creature =<< opponentTarget)

    , (beast's_enrage,
          do (l,d) <- findBeast beast_wolverine
             let maxLife = d ^. deckCardOrig.creatureCard.creatureLife
                 d1 = d & deckCard.creatureCard.creatureLife .~ maxLife
                        & deckCardChangeAttack 2
             updGame_ (creatureAt l .~ Just d1))
    , (beast's_natural_healing,
        forM_ (slotsFor Caster) $ \s -> creatureChangeLife_ s 18)
    , (beast's_breathe_fire, damageSpell $ \dmg ->
        do (_,d) <- findBeast beast_ancient_dragon
           doWizardDamage Opponent d 10
           damageCreatures Effect (dmg 10) (slotsFor Opponent))


    -- Goblin Spells --------------------------
    , (goblin's_rescue_operation,
         do lFrom <- target
            mbTo <- randomBlankSlot (locWho lFrom)
            lTo <- case mbTo of
                     Nothing -> stopError "No free slots"
                     Just lTo  -> return lTo
            creatureMove lFrom lTo
            when (locWho lFrom == Caster) (creatureChangeLife_ lTo 5))
    , (goblin's_army_of_rats, damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 12) (slotsFor Opponent)
           mbL <- randomCreature Caster
           case mbL of
             Nothing -> return ()
             Just l  -> damageCreature Effect (dmg 12) l)

    -- Chaos spells -----------------------------
    , (chaos_doom_bolt, damageSpell $ \dmg ->
          do tgt <- randomCreature Opponent
             case tgt of
              Nothing  -> return ()
              Just loc -> damageCreature Effect (dmg 25) loc)
    , (chaos_chaotic_wave, damageSpell $ \dmg ->
        do forM_ (slotsFor Opponent) $ \l ->
                 do amt <- random (randInRange 2 12)
                    damageCreature Effect (dmg (fromIntegral amt)) l

           forM_ (slotsFor Caster) $ \l ->
                 do amt <- random (randInRange 2 12)
                    creatureChangeLife_ l amt)

    -- Sorcery spells ----------------------------
    , (sorcery_healing_spray,
        do tgt <- casterTarget
           creatureChangeLife_ tgt 9
           creatureChangeLife_ (rightOf tgt) 6
           creatureChangeLife_ (leftOf tgt) 6)
    , (sorcery_fireball, damageSpell $ \dmg ->
        do tgt <- opponentTarget
           damageCreature Effect (dmg 9) tgt
           damageCreatures Effect (dmg 6) $ [leftOf tgt, rightOf tgt])
    , (sorcery_steal_essence, damageSpell $ \dmg ->
        do tgt <- opponentTarget
           damageCreature Effect (dmg 5) tgt
           whenCreature tgt $ \cr -> 
            if cr ^. deckCardLife <= 0
              then wizChangePower Caster Special 4
              else return ())
    , (sorcery_sacrifice, 
      do tgt <- casterTarget
         creatureDestroy tgt
         forM_ [Fire, Water, Air, Earth] $ \elt -> wizChangePower Caster elt 3)
    , (sorcery_ritual_of_glory,
        do forM_ (slotsFor Caster) $ \l ->
            whenCreature l $ \cr ->
              do creatureChangeLife_ l 100000
                 updGame_ $ creatureAt l . mapped 
                    .~ deckCardAddMod (AttackBoost 3, UntilEndOfTurn 0) cr)
    , (sorcery_mana_burn, damageSpell $ \dmg ->
        do g <- getGame
           let eltList = [ (g ^. player Opponent . playerPower e, e)
                         | e <- allElements ]
               (amt, elt) = maximumBy (compare `on` fst) eltList

           damageCreatures Effect (dmg (fromIntegral amt)) (slotsFor Opponent)
           wizChangePower Opponent elt (-3))

    , (sorcery_sonic_boom, damageSpell $ \dmg ->
        do doWizardDamage Opponent c (dmg 11)
           forM_ (slotsFor Opponent) $ \l ->
              do damageCreature Effect (dmg 11) l
                 creatureSkipNextAttack l)

    , (sorcery_disintegrate, damageSpell $ \dmg ->
        do tgt <- opponentTarget
           creatureDestroy tgt
           damageCreatures Effect (dmg 11) (delete tgt $ slotsFor Opponent))

    -- Forest spells
    , (forest_ritual_of_the_forest,
        do atk <- Decks.Forest.getRabbitAttack Caster
           let amt = 5 + atk
           wizChangeLife Caster amt
           forM_ (slotsFor Caster) $ \l -> creatureChangeLife_ l amt)


      -- Demonic Spells
    , (demonic_explosion, damageSpell $ \dmg ->
        do tgt <- casterTarget
           creatureDestroy tgt
           damageCreature Effect (dmg 28) (oppositeOf tgt))

    , (demonic_power_chains, damageSpell $ \dmg ->
        do tgt <- opponentTarget
           damageCreature Effect (dmg 12) tgt
           mb <- getCreatureAt tgt
           case mb of
              Nothing -> stopError "Spell must have a target"
              Just a | a ^. deckCardElement == Special ->
                stopError "Power chains must target a base creature"
                     | otherwise -> 
                        wizChangePower Opponent (a ^. deckCardElement) (-3))

    , (demonic_hellfire, damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 13) $ slotsFor Opponent
           (_,deaths) <- countLiving (slotsFor Opponent)
           wizChangePower Caster Fire deaths)


    -- Control Spells
    , (control_poisonous_cloud, damageSpell $ \dmg ->
        do forM_ allElements $ \p -> wizChangePower Opponent p (-1)
           g <- getGame
           let opp = inhabitedSlots g (slotsFor Opponent)
           forM_ opp $ \(cl, cc) ->
             do let life    = cc ^. deckCardLife
                    baseDmg = div (life + 1) 2    -- half round up
                    damage  = dmg (fromIntegral baseDmg)
                damageCreature Effect damage cl)

    , (control_weakness, damageSpell $ \dmg ->
        do forM_ allElements (\e -> wizChangePower Opponent e (-1))
           doWizardDamage Opponent c (dmg 3))

    -- Golem Spells
    , (golem_golems_frenzy, damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 3) (slotsFor Opponent)
           (_,deaths) <- countLiving (slotsFor Opponent)
           (l,_) <- Decks.Golem.getGolem
           creatureTemporaryAttackBoost l (deaths * 3))

    , (golem_golems_justice, damageSpell $ \dmg ->
        do damageCreatures Effect (dmg 4) (slotsFor Opponent)
           (l,_) <- Decks.Golem.getGolem
           creatureChangeLife_ (leftOf l) 4
           creatureChangeLife_ (rightOf l) 4)

    , (golem_army_upgrade,
        do for_ (slotsFor Caster) (`creatureChangeLife_` 3)
           (l,_) <- Decks.Golem.getGolem
           creatureChangeAttack l 2)

    -- Spirit Spells
    , (spirit_divine_meddling, damageSpell $ \dmg ->
        do forM_ [ Fire, Air, Earth, Water ] $ \el -> wizChangePower Caster el 2
           doWizardDamage Opponent c (dmg 10))
    , (spirit_divine_justice, damageSpell $ \dmg ->
        do tgt <- casterTarget
           creatureChangeLife_ tgt 12
           damageCreatures Effect (dmg 12) (delete tgt allSlots))
    , (spirit_rage_of_god, damageSpell $ \dmg ->
         do damageCreatures Effect (dmg 12) (slotsFor Opponent)
            (lived,_) <- countLiving (slotsFor Opponent)
            when (lived > 0) $
              doWizardDamage Opponent c (dmg (fromIntegral (3 * lived))))

    -- Vampiric
    , (vampiric_blood_boil, damageSpell $ \dmg ->
         do damageCreatures Effect (dmg 4) (slotsFor Opponent)
            (_,deaths) <- countLiving (slotsFor Opponent)
            when (deaths > 0) $ wizChangePower Caster Special deaths)

    ]

-- | Activate actions on creatures that just died, and possibly end the game.
checkDeath :: GameM ()
checkDeath =
  do checkGameWon
     mapM_ checkCreature (slotsFor Opponent)
     mapM_ checkCreature (slotsFor Caster)
  where
  checkCreature l =
    whenCreature l $ \c ->
      when (c ^. deckCardLife <= 0) (creatureKill l)










-- | Actions taken by another creature, when a creature is summoned.
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
          $ damageCreature Effect 4 sl
      )

    , (goblin's_goblin_hero, \(cl,_) tgt ->
        when (cl == oppositeOf tgt) $
          do mb <- randomBlankSlot (locWho cl)
             case mb of
               Nothing -> return () -- oh no, we have nowhere to run!
               Just l  -> creatureMove cl l)

    , (spirit_templar, \(cl,c) sl ->
        when (isNeighbor cl sl) (doWizardDamage (theOtherOne (locWho cl)) c 4))
    ]


creatureDied ::
  Location {- ^ actor -} ->
  Location {- ^ where the killed creature was (it is not there anymore) -} ->
  GameM ()
creatureDied = creatureReact
  [ (death_keeper_of_death, \(cl,_) dl ->
       do let owner = locWho cl
          when (owner /= locWho dl) $    -- opponents creature died
            wizChangePower owner Special 1
    )
  , (goblin's_goblin_looter, \(cl,_) _ ->
      do el <- randomPower
         wizChangePower (locWho cl) el 1)


  , (spirit_holy_avenger, \(cl,_) died ->
      when (isNeighbor cl died) (creatureChangeAttack cl 2))

  -- Vampiric
  , (vampiric_ghoul, \(cl,_) died ->
      when (locWho died == theOtherOne (locWho cl)) $
        creatureChangeAttack cl 1)

  ]



damageCreatures :: DamageSource -> Int -> [Location] -> GameM ()
damageCreatures dmg amt = mapM_ (damageCreature dmg amt)

-- | The creature at this location (if any) should take some damage,
-- if it wishes to.
damageCreature :: DamageSource -> Int -> Location -> GameM ()
damageCreature dmg amt l =
  whenCreature l $ \c ->
    case Map.lookup (deckCardName c) abilities of
      Just act -> act
      Nothing  -> doDamage amt
  where
  abilities = Map.fromList
    [ (water_giant_turtle, doDamage (amt - 5))
    , (water_ice_golem, case dmg of
                          Attack _ -> doDamage amt
                          _        -> return ())


    , (mechanical_steel_golem, case dmg of
                                 Attack _ -> doDamage (amt - 1)
                                 _        -> return())


    , (illusion_phantom_warrior, doDamage (min 1 amt))
    , (illusion_wall_of_reflection,
      do dmgDone <- doDamage' amt
         Just c <- getCreatureAt l
         doWizardDamage (theOtherOne $ locWho l) c dmgDone)

    , (forest_angry_angry_bear,
        do when (amt > 0) (creatureChangeAttack l 1)
           doDamage amt)


    , (control_mindstealer,
        case dmg of
          Attack l1 | isOpposing l l1 -> damageCreature (Attack l1) amt l1
          _ -> doDamage amt)

    , (golem_guardian_statue,
         do ms <- map fst <$> creatureGetMods l
            unless (Immune `elem` ms) (doDamage amt))

    , (other_golem,
        case dmg of
          Attack {} -> doDamage amt
          _         -> return ())

    , (other_initiate,
        do let check n =
                 do mb <- getCreatureAt n
                    case mb of
                      Just cr -> return (deckCardName cr ==
                                                      vampiric_vampire_elder)
                      Nothing -> return False
           protectedL <- check (leftOf l)
           protectedR <- check (rightOf l)
           unless (protectedL || protectedR) (doDamage amt))
    ]

  -- Additive changes to damage
  changeAdd = Map.fromList
    [ (holy_holy_guard, \me -> if isNeighbor l me then -2 else 0)
    ]

  -- Multiplicative changes to damage
  changeMul = Map.fromList
    [ (vampiric_justicar, \me -> if isOpposing l me then 2 else 1)
    ]


  -- Compute how location `lc` affects the damage that we should do.
  otherDamageMod g base mp lc =
    fromMaybe base $
      do cr <- g ^. creatureAt lc
         f  <- Map.lookup (deckCardName cr) mp
         return (f lc)


  doDamage am = doDamage' am >> return ()

  doDamage' am0 =
    do g <- getGame
       let slots x y = map (otherDamageMod g x y) (delete l allSlots)
           dmg1  = sum     (am0  : slots 0 changeAdd)
           dmg2  = product (dmg1 : slots 1 changeMul)
       creatureChangeLife l (negate dmg2)



-- | Call this whenever a creatures leaves the board (either death
-- or destruction).  We use it to notify the UI that this happen,
-- and also to do restoration stuff (e.g., in the case for beasts
-- switch from ability back to beast).
-- Assumes that the creature has already been removed from the board.
creatureLeave :: (Location, DeckCard) -> GameM ()
creatureLeave (l,d) =
  do addLog (CreatureDie l)
     case Map.lookup (deckCardName d) abilities of
       Just act -> act
       Nothing  -> return ()
  where
  abilities = Map.fromList (map beastDied (Map.toList Decks.Beast.beastAbilityMap))

  beastDied (b,ab) = (b, updGame_ $ replaceCard (locWho l) ab
                                  $ newDeckCard Special (d ^. deckCardOrig))




-- | Remove a creature from the game, without activating its death handler.
creatureDestroy :: Location -> GameM ()
creatureDestroy l =
  whenCreature l $ \c ->
    case Map.lookup (deckCardName c) abilities of
      Nothing   -> do updGame_ (creatureAt l .~ Nothing)
                      creatureLeave (l,c)
      Just act  -> act
  where
  abilities = Map.fromList
    [ (other_golem, creatureKill l)
    ]

-- | Remove a creature from the game, and invoke its death handler, if any.
creatureKill :: Location -> GameM ()
creatureKill l =
  whenCreature l $ \c ->
    do updGame_ (creatureAt l .~ Nothing)
       Map.findWithDefault (creatureLeave (l,c)) (deckCardName c) (abilities c)
       mapM_ (`creatureDied` l) allSlots

  where
  abilities c = Map.fromList $
    let leave = creatureLeave (l,c)
    in
    [
      -- Air
      (air_phoenix,
         do leave
            wizUpd_ (locWho l) $ \p ->
              if p ^. playerPower Fire >= 10
                then let newCard = c & deckCard .~ (c ^. deckCardOrig)
                     in  p & creatureInSlot (locWhich l) .~ Just newCard
                 else p)

    -- Holy
    , (holy_monk, wizChangePower (locWho l) Special 2 >> leave)

    -- Forest
    , (forest_bee_queen, doWizardDamage (theOtherOne (locWho l)) c 3 >> leave)
    , (other_bee_soldier, doWizardDamage (theOtherOne (locWho l)) c 3 >> leave )

    -- Demonic
    , (demonic_lemure,
        let lr = newDeckCard Special
                   (getCard other_cards other_scrambled_lemure)
         in leave >> summonCreature lr l)
    , (demonic_ergodemon,
        do forM_ allElements $ \elt ->
             wizChangePower (theOtherOne $ locWho l) elt (-1)
           leave
       )
    , (demonic_demon_quartermaster,
        let sq = newDeckCard Special
                    (getCard other_cards other_enraged_quartermaster)
         in leave >> summonCreature sq l)
    , (demonic_threeheaded_demon,
        let da = newDeckCard Special (getCard other_cards other_demon_apostate)
         in leave >> summonCreature da l)

    -- Golem
    , (other_golem,
         do leave
            let newGolem = c & deckCardLife .~
                           c ^. deckCardOrig . creatureCard . creatureLife
            Just newLoc <- randomBlankSlot (locWho l)
            summonCreature newGolem newLoc
            doSomething newLoc
            doWizardDamage (locWho newLoc) c 10)

    -- Vampiric
    , (vampiric_devoted_servant,
        case c ^. deckCard . creatureCard . creatureAttack of
          Just n -> wizChangePower (locWho l) Special n
          Nothing -> return ())
    ]



--------------------------------------------------------------------------------

-- | The creature at the given location performs its attack, if any.
creaturePerformAttack :: Location -> GameM ()
creaturePerformAttack l =
  do g <- getGame
     case g ^. creatureAt l of
       Nothing -> return ()
       Just c
         | isWall c
        || not (c ^. deckCardEnabled)
        || SkipNextAttack `elem` map fst (c ^. deckCardMods) -> return ()

         | otherwise  ->
           do diabledByHorror <- checkHorrors c
              unless diabledByHorror $
                do addLog (CreatureAttack l)
                   let p = getAttackPower g (l,c)
                   case Map.lookup (deckCardName c) abilities of
                     Just act -> act c p
                     Nothing  -> normalAttack c p
                   creatureRmMod l ((UntilNextAttack ==) . snd)
                   checkDeath
  where
  otherWizard = theOtherOne (locWho l)

  normalAttack c p =
    do g <- getGame
       let loc = oppositeOf l
       case g ^. creatureAt loc of
         Nothing -> doWizardDamage otherWizard c p
         Just _  -> damageCreature (Attack l) p loc

  checkHorrors c =
    do hs <- findCreature Opponent control_ancient_horror
       if null hs
          then return False
          else do p <- withGame (view (player Opponent . playerPower Special))
                  return (c ^. deckCard . cardCost < p)


  abilities = Map.fromList
    [ (earth_hydra, damageEveryone)
    , (earth_forest_sprite, damageEveryone)
    , (air_lightning_cloud, damageEveryone)
    , (death_master_lich, \c p ->
        do let opp = oppositeOf l
           mb <- getCreatureAt opp
           case mb of
             Nothing ->
               do damaged <- doWizardDamage' otherWizard c p
                  when damaged (wizChangePower (locWho l) Special 2)
             _ -> damageCreature (Attack l) p opp)
    , (demonic_threeheaded_demon, damageEveryone)
    , (other_golem, \c p ->
        do extra_attacks <- length <$> findCreature Caster golem_golem_handler
           replicateM_ (1 + extra_attacks) (normalAttack c p)
      )
    ]

  damageEveryone c p =
    do doWizardDamage otherWizard c p
       damageCreatures (Attack l) p (slotsFor otherWizard)




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
doWizardDamage' who dc amt0 =

  checkWhiteElephant $
  do checkGoblinSaboteur
     amt1 <- checkIceGuard =<< checkJusticar
     checkVampireMystic
     checkVampireChastiser
     wizChangeLife who (negate amt1)
     return (amt1 > 0)

  where
  checkWhiteElephant k =
    do els <- findCreature who beast_white_elephant
       case els of
         [] -> k
         (l,_) : _ -> do damageCreature Effect amt0 l
                         return False
  checkVampireMystic =
    do vms <- findCreature (theOtherOne who) vampiric_vampire_mystic
       forM_ vms $ \(l,_) -> creatureMod l (AttackBoost 2) (UntilEndOfTurn 0)

  checkVampireChastiser =
    do cs <- findCreature who vampiric_chastiser
       forM_ cs $ \(l,_) -> creatureMod l (AttackBoost 2) UntilNextAttack

  checkGoblinSaboteur =
    when (deckCardName dc == goblin's_goblin_saboteur) $
    do g <- getGame
       let p   = g ^. player who
           els = Map.keys (Map.filter (not . null) (p ^. playerDeck))
       unless (null els) $
         do el <- random (oneOf els)
            updGame_ (player who . playerDeck . at el %~ fmap tail)

  checkJusticar =
    do xs <- findCreature (theOtherOne who) vampiric_justicar
       n  <- (length . catMaybes) <$> mapM (getCreatureAt . oppositeOf . fst) xs
       return (amt0 + 2 * n)

  checkIceGuard amt =
    do g <- getGame
       let halfRoundUp _ x = div (x + 1) 2
       return $ foldr halfRoundUp amt
              $ filter ((water_ice_guard ==) . deckCardName)
              $ map snd
              $ inhabitedSlots g (slotsFor who)

-- | Compute the current attack power for the given creature.
getAttackPower :: Game -> (Location, DeckCard) -> Int
getAttackPower g (l,c) = max 0 (base + boardChange + modChange)

  where
  ourCreatures   = inhabitedSlots g (slotsFor Caster)
  theirCreatures = inhabitedSlots g (slotsFor Opponent)

  boardChange = sum $ map (creatureModifyAttack (l,c))
                    $ ourCreatures ++ theirCreatures

  modChange = sum [boostModVal m | m <- c ^. deckCardMods]

  boostModVal (AttackBoost k,_) = k
  boostModVal _ = 0

  name  = deckCardName c
  owner = locWho l

  base = case c ^. deckCard.creatureCard.creatureAttack of
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

  elemental s = g ^. player owner . playerPower s



-- | Compute changes to the attack value of a speicific creature.
creatureModifyAttack :: (Location,DeckCard) {- ^ Attack of this -} ->
                        (Location,DeckCard) {- ^ Modifier of attack -} -> Int

creatureModifyAttack (l,d) (l1,c)
  | isWall d = 0
  | name == fire_orc_chieftain && isNeighbor l l1 = 2
  | name == fire_minotaur_commander && ours && l /= l1 = 1
  | name == golem_golem_instructor && ours && deckCardName d == other_golem = 2
  | deckCardName d == goblin's_goblin_hero && isNeighbor l l1 = 2
  | otherwise = 0
  where
  name = deckCardName c
  ours = sameSide l l1

creatureModifySpellDamageAdd ::
  Who      {- ^ Whose spell are we modifying -} ->
  DeckCard {- ^ Summoned crature modifying -} ->
  Int
creatureModifySpellDamageAdd who c =
  Map.findWithDefault (\_ -> 0) (deckCardName c) abilities $ who
  where
  abilities = Map.fromList $
      map (\(nm, w, a) -> (nm, \t -> if t == w then a else 0)) 
      [ (air_faerie_apprentice, Caster, 1)
      , (forest_treefolk_protector, Opponent, -3)
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
creatureEndOfTurn :: Location -> GameM ()
creatureEndOfTurn l =
  whenCreature l $ \c ->
    case Map.lookup (deckCardName c) abilities of
      Nothing  -> return ()
      Just act -> do doSomething l
                     act
  where
  abilities = Map.fromList
    [ (beast_basilisk,
        forM_ (slotsFor Opponent) $ \sl ->
          whenCreature sl $ \cr ->
            when (cr ^. deckCardLife <= 8) $
              damageCreature Effect 4 sl)
    , (goblin's_portal_jumper,
         do creatureSkipNextAttack (oppositeOf l)
            mbNew <- randomBlankSlot (locWho l)
            case mbNew of
              Nothing   -> return ()
              Just lTo  -> creatureMove l lTo)
    , (chaos_insanian_king,
         do let sld = newDeckCard Special (getCard other_cards other_soldier)
            mbNew <- randomBlankSlot (locWho l)
            case mbNew of
              Nothing  -> return ()
              Just slt -> summonCreature sld slt)
    ]



getCreatureEffects :: DeckCard -> Maybe CreatureEffects
getCreatureEffects c = Map.lookup (deckCardName c) newCreatureAPI
  where
  newCreatureAPI =
    Map.unions
      [ Decks.Fire.creatures
      , Decks.Water.creatures
      , Decks.Air.creatures
      , Decks.Earth.creatures
      , Decks.Death.creatures
      , Decks.Holy.creatures
      , Decks.Mechanical.creatures
      , Decks.Illusion.creatures
      , Decks.Beast.creatures
      , Decks.Goblin.creatures
      , Decks.Demonic.creatures
      , Decks.Golem.creatures
      , Decks.Spirit.creatures
      , Decks.Vampiric.creatures
      , Decks.Control.creatures
      ]





-- | Playe a card---either spell or summon creature.
playCard :: DeckCard -> Maybe Location -> GameM ()
playCard c mbLoc =
  case (c ^. deckCard . cardType , mbLoc) of
    (Spell {}, mb) -> do cost <- checkCost c
                         castSpell c mb
                         payCost cost
                         checkDeath
    (Creature {}, Just l)
      | locWho l == Caster && locWhich l >= 0 && locWhich l < slotNum ->
        do g <- getGame
           let isEmissary = deckCardName c == death_emissary_of_dorlak
               isWolf     = deckCardName c == forest_forest_wolf
           -- XXX: refactor me
           case g ^. creatureAt l of
             Nothing | isEmissary ->
                       stopError "Emissary must be played on top of a creature"
                     | isWolf     ->
                       stopError "Wolf must be played on top of a rabbit"
             Just r | not isEmissary && not isWolf ->
                      stopError "Creature must be played on an empty space"
                    | isWolf && deckCardName r /= other_magic_rabbit ->
                      stopError "Wolf must be played on top of a rabbit"
                    | isWolf      ->
                          let ratk = r ^. deckCard
                                        . creatureCard . creatureAttack
                              wlf = c & deckCard . creatureCard
                                      . creatureAttack .~ ratk
                          in doSummon wlf l
             _ -> do doSummon c l
    _ -> stopError "Card needs an approprate target"

  where
  el = c ^. deckCardElement

  payCost cost = do wizChangePower Caster el (negate cost)
                    vamp <- isCurVampire
                    when (vamp && el == Special) $
                      wizChangeLife Caster ((-2) * c ^. deckCardOrig . cardCost)

  doSummon dc l = do cost <- checkCost c

                     -- Normally, this should be empty, but in the
                     -- case of Dorlak or the wolf, we should first destroy
                     -- the creature, and perform death effects, if any.
                     creatureDestroy l

                     let c1 = dc & deckCardEnabled .~ False
                     summonCreature c1 l
                     creatureSummonEffect (l,c1)
                     payCost cost
                     checkDeath
                     mapM_ (`creatureSummoned` l) allSlots
                     checkDeath

  creatureSummonEffect (l,c1) =
    case getCreatureEffects c1 of
      Just eff -> onSummoned eff l
      Nothing  -> return ()

  checkCost c1 =
    do g <- getGame
       let base = c1 ^. deckCard . cardCost
           cost = base + extraCost g
           have = g ^. player Caster . playerPower (c1 ^. deckCardElement)
       when (cost > have) (stopError "Card needs more power")
       return cost

    where
    extraCost g = sum
                $ map (creatureModifyCost . snd)
                $ inhabitedSlots g
                $ slotsFor Opponent

    creatureModifyCost dc =
      case getCreatureEffects dc of
        Just eff -> modifyCost eff c1
        Nothing  -> 0







isCurVampire :: GameM Bool
isCurVampire =
  (vampiric_cards ==) <$> withGame (view (player Caster . playerClass))

