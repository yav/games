{-# Language OverloadedStrings #-}
module Spell where

import Common
import Rule
import Deed

spells :: [Deed]
spells = [] {-
  [ spellDeed Red
    "Fireball"
    [ produces (5 *** Attack Ranged Fire) ]

    "Firestorm"
    [ produces (GainWound, 8 *** Attack Siege Fire) ]

  , spellDeed Red
    "Flame Wall"
    [ produces (5 *** Attack Melee Fire)
    , produces (7 *** Block Fire) ]

    "Flame Wave"
    [ produces (5 *** Attack Melee Fire, perEnemy (2 *** Attack Melee Fire))
    , produces (7 *** Block Fire, perEnemy (2 *** Block Fire)) ]

  , spellDeed Red
    "Demolish"
    [ produces $ ChangeEnemy All
               $ EnemyIf [ EnemyResists Fire False ]
               $ EnemyAnd [ LooseCiteFortifications, LooseArmor 1 1 ] ]

    "Disintegrate"
    [ onlyIf InAttackPhase &&&
      produces ( ChangeEnemy One $
                  EnemyIf [ EnemyResists Fire False ] EnemyDestroy
               , ChangeEnemy All $
                  EnemyIf [ EnemyResists Fire False ] (LooseArmor 1 1)) ]

   -- XXX: Burning shield: how to handle if "blocked?"
   -- XXX: Mana meltdown (interactive)



  , spellDeed Green
    "Tremor"
    [ produces $ ChangeEnemy One $ LooseArmor 3 1
    , produces $ ChangeEnemy All $ LooseArmor 2 1 ]

    "Earthquake"
    [ produces $ ChangeEnemy One $ EnemyAnd
                  [ LooseArmor 3 1
                  , EnemyIf [ EnemyIsForitified ] $ LooseArmor 3 1 ]
    , produces $ ChangeEnemy All $ EnemyAnd
                  [ LooseArmor 2 1
                  , EnemyIf [ EnemyIsForitified ] (LooseArmor 2 1) ] ]

  , spellDeed Green
    "Underground Travel"
    [ produces (specialMove 3 [Swamp,Lake,Ocean] EndMoveSafe) ]

    "Underground Attack"
    [ produces (specialMove 3 [Swamp,Lake,Ocean] EndMoveAttack) ]

   , spellDeed Green
    "Restoration"
    [ produces (3 *** Healing)
    , OnTerrain Forest --> 5 *** Healing ]

    "Rebirth"
    [ produces (3 *** Healing, 3 *** Rebirth)
    , OnTerrain Forest --> (5 *** Healing, 5 *** Rebirth) ]

    -- XXX: Meditation
    -- XXX: energy_flow (interactive)



  , spellDeed Blue
    "Snowstorm"
    [ produces (5 *** Attack Ranged Ice) ]

    "Blizzard"
    [ produces (GainWound, 8 *** Attack Siege Ice) ]

  , spellDeed Blue
    "Mana Bolt"
    [ Blue  --> 8 *** Attack Melee Ice
    , Red   --> 7 *** Attack Melee ColdFire
    , White --> 6 *** Attack Ranged Ice
    , Green --> 5 *** Attack Siege Ice ]

    "Mana Thunderbolt"
    [ Blue  --> 11 *** Attack Melee Ice
    , Red   --> 10 *** Attack Melee ColdFire
    , White -->  9 *** Attack Ranged Ice
    , Green -->  8 *** Attack Siege Ice ]

  , spellDeed Blue
    "Chill"
    [ produces $ ChangeEnemy One
               $ EnemyIf [ EnemyResists Ice False ]
               $ EnemyAnd [ NoAttack, LooseResistance Fire ] ]
    "Lethal Chill"
    [ produces $ ChangeEnemy One
               $ EnemyIf [ EnemyResists Ice False ]
               $ EnemyAnd [ NoAttack, LooseArmor 4 1 ] ]

    -- XXX: Space bending
    -- XXX: Mana Claim (interactive)


  , spellDeed White
    "Wings of Wind"
    [ produces $ specialMove 5 [] EndMoveSafe ]

    "Wings of Night"
    (let wings c = c *** Movement --> (ChangeEnemy One NoAttack
                                     , ARule (wings (c+1)))
     in [ wings 0 ]
    )

  , spellDeed White
    "Expose"
    [ produces ( ChangeEnemy One $ EnemyAnd
                                 $ LooseFortifications
                                 : [ LooseResistance e
                                        | e <- [ Physycal, Fire, Ice ] ]
               , 2 *** Attack Ranged Physycal ) ]

    "Mass Expose"
    [ produces ( ChangeEnemy All $ EnemyAnd
                                 $ LooseFortifications
                                 : [ LooseResistance e
                                        | e <- [ Physycal, Fire, Ice ] ]
               , 3 *** Attack Ranged Physycal ) ]

  , spellDeed White
    "Whirlwind"
    [ produces $ ChangeEnemy One NoAttack ]

    "Tornado"
    [ onlyIf InAttackPhase &&& produces (ChangeEnemy One EnemyDestroy) ]

  , spellDeed White
    "Call to Arms"
    [ produces UseUnhiredUnit ]

    "Call to Glory"
    [ produces RecruitUnit ]

    -- XXX: Mind Read (interactive)

  ]

-}

