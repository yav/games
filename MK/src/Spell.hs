{-# Language OverloadedStrings #-}
module Spell where

import Common
import Rule
import Deed

spells :: [Deed]
spells =
  [ spellDeed Red
    "Fireball"
    [ produces (5 *** Attack Ranged Fire) ]

    "Firestorm"
    [ produces (GainWound, 8 *** Attack Siege Fire) ]


  , spellDeed Blue
    "Snowstorm"
    [ produces (5 *** Attack Ranged Ice) ]

    "Blizzard"
    [ produces (GainWound, 8 *** Attack Siege Ice) ]


  , spellDeed White
    "Expose"
    [ produces ( ChangeEnemy One ( LooseFortifications
                                 : [ LooseResistance e
                                        | e <- [ Physycal, Fire, Ice ] ])
               , 2 *** Attack Ranged Physycal ) ]

    "Mass Expose"
    [ produces ( ChangeEnemy All ( LooseFortifications
                                 : [ LooseResistance e
                                        | e <- [ Physycal, Fire, Ice ] ])
               , 3 *** Attack Ranged Physycal ) ]


  , spellDeed Green
    "Tremor"
    [ produces (ChangeEnemy One [ LooseArmor 3 1 ])
    , produces (ChangeEnemy All [ LooseArmor 2 1 ]) ]

    "Earthquake"
    [ produces (ChangeEnemy One
                  [ LooseArmor 3 1
                  , enemyIf [ EnemyIsForitified ] (LooseArmor 3 1) ])
    , produces (ChangeEnemy All
                  [ LooseArmor 2 1
                  , enemyIf [ EnemyIsForitified ] (LooseArmor 2 1) ]) ]


  , spellDeed Red
    "Flame Wall"
    [ produces (5 *** Attack Melee Fire)
    , produces (7 *** Block Fire) ]

    "Flame Wave"
    [ produces (5 *** Attack Melee Fire, perEnemy (2 *** Attack Melee Fire))
    , produces (7 *** Block Fire, perEnemy (2 *** Block Fire)) ]


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


  , spellDeed White
    "Whirlwind"
    [ produces (ChangeEnemy One [NoAttack]) ]

    "Tornado"
    [ onlyIf InAttackPhase &&& produces (ChangeEnemy One [ EnemyDestroy ]) ]


  , spellDeed Green
    "Underground Travel"
    [ produces (specialMove 3 [Swamp,Lake,Ocean] EndMoveSafe) ]

    "Underground Attack"
    [ produces (specialMove 3 [Swamp,Lake,Ocean] EndMoveAttack) ]


  ]



