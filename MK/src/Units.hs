{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module Units where

import Rule
import Common

import Util.Perhaps

import           Data.Text (Text)
import           Data.Set  (Set)
import qualified Data.Set as Set
import           Data.Maybe (listToMaybe)


-- | A unit hired by a player.
data ActiveUnit = ActiveUnit
  { baseUnit    :: Unit
  , unitReady   :: Bool
  , unitWounds  :: Int
  , unitAssignedDamageThisCombat :: Bool
  }

-- | Make a new active, when freshly hired from somewhere.
activeateUnit :: Unit -> ActiveUnit
activeateUnit u = ActiveUnit
  { baseUnit = u
  , unitReady = True
  , unitWounds = 0
  , unitAssignedDamageThisCombat = False
  }

-- | Make any updates necessary at the end of a combat.
endOfCombat :: ActiveUnit -> ActiveUnit
endOfCombat u = u { unitAssignedDamageThisCombat = False }

-- | Assign some type of damage to a unit.
unitAssignDamage :: Int {-^ Amount -} ->
                    DamageInfo ->
                    ActiveUnit  {-^ Unit to be damaged -} ->
                    Perhaps (Int, Maybe ActiveUnit)
                    -- ^ Remaining damage and updated unit.
                    -- If the unit is paralized, we return 'Nothing'.
unitAssignDamage damage DamageInfo { .. } ActiveUnit { .. }

  | damage < 1 =
    Failed "Units may be assigned only positive amount of damage."

  | unitWounds > 0 =
    Failed "The unit is already wounded."

  | unitAssignedDamageThisCombat =
    Failed "The unit was already assigned damage this combat."

  | otherwise =
    let resistant = damageElement `Set.member` unitResists baseUnit
        armor     = unitArmor baseUnit
        absorbed  = if resistant then 2 * armor else armor
        wounds    = if damagePoisons then 2 else 1
        actualWounds = if resistant && damage <= armor then 0 else wounds
    in Ok ( max 0 (damage - absorbed)
          , if actualWounds > 0 && damageParalyzes
              then Nothing
              else Just ActiveUnit { unitAssignedDamageThisCombat = True
                                   , unitWounds = actualWounds, .. }
          )

data UnitType = RegularUnit | EliteUnit

-- | A unit that has not been hired (i.e., in the offer)
data Unit = Unit { unitName       :: Text
                 , unitType       :: UnitType
                 , unitLevel      :: Int
                 , unitCost       :: Int
                 , unitArmor      :: Int
                 , unitResists    :: Set Element
                 , unitSource     :: Set UnitSource
                 , unitAbilities  :: [Rule]
                 }

data UnitSource =
    FromVillage
  | FromMonastery
  | FromKeep
  | FromMageTower
  | FromCity
    deriving (Eq,Ord,Show)


findUnit :: Text -> Maybe Unit
findUnit x =
  listToMaybe [ u | u <- regularUnits ++ eliteUnits, unitName u == x ]


regularUnits :: [Unit]
regularUnits =
  concat
  [ replicate 3 $
    Unit { unitName       = "Peasants"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 4
         , unitArmor      = 3
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage ]
         , unitAbilities  =
            [ produces (2 *** Attack Melee Physycal)
            , produces (2 *** Block Physycal)
            , produces (2 *** Influence)
            , produces (2 *** Movement)
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Foresters"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 5
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.singleton FromVillage
         , unitAbilities  =
            [ produces (2 *** Movement) &&&
              produces [ ChangeTerrainCost t (DecreaseBy 1 0)
                                          | t <- [ Forest, Hills, Swamp ] ]
            , produces (3 *** Block Physycal)
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Herbalists"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 3
         , unitArmor      = 2
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromMonastery ]
         , unitAbilities  =
            [ Green --> 2 *** Healing
            , produces (ChangeUnit One [UnitReadyLevel 1])
            , produces (ChangeUnit One [UnitReadyLevel 2])
            , produces Green
            ]
         }




  , replicate 1 $
    Unit { unitName       = "Northern Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , Blue --> 4 *** Attack Melee Ice
            , Blue --> 4 *** Block Ice
            ]
         }

  , replicate 1 $
    Unit { unitName       = "Red Cape Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , Red --> 4 *** Attack Melee Fire
            , Red --> 4 *** Block Fire
            ]
         }

  , replicate 1 $
    Unit { unitName       = "Savage Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , Green --> 4 *** Attack Siege Physycal
            ]
         }


  , replicate 2 $
    Unit { unitName       = "Utem Crossbowmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 6
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromKeep ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , produces (2 *** Attack Ranged Physycal)
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Utem Guardsmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 5
         , unitArmor      = 5
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromKeep ]
         , unitAbilities  =
            [ produces (2 *** Attack Melee Physycal)
            , produces (4 *** SwiftBlock Physycal)
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Utem Swordsmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 6
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , produces (6 *** Attack Melee Physycal,
                                             ChangeUnit Self [UnitGainWound])
            , produces (6 *** Block Physycal,ChangeUnit Self [UnitGainWound])
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Guardian Golems"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 3
         , unitResists    = Set.singleton Physycal
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  =
            [ produces (2 *** Attack Melee Physycal)
            , produces (2 *** Block Physycal)
            , Red --> 4 *** Block Fire
            , Blue --> 4 *** Block Ice
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Illusionists"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 2
         , unitResists    = Set.singleton Physycal
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = []
         }
  ]

eliteUnits :: [Unit]
eliteUnits =
  concat
  [ replicate 3 $
    Unit { unitName       = "Altem Guardians"
         , unitType       = EliteUnit
         , unitLevel      = 4
         , unitCost       = 11
         , unitArmor      = 7
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromCity ]
         , unitAbilities  = [ produces (5 *** Attack Melee Physycal)
                            , produces (8 *** SwiftBlock Physycal)
                            , Green --> ChangeUnit All
                                          [ UnitGainResistance r
                                            | r <- [Physycal,Fire,Ice] ]
                            ]
         }

  , replicate 2
    $ Unit { unitName     = "Altem Mages"
         , unitType       = EliteUnit
         , unitLevel      = 4
         , unitCost       = 12
         , unitArmor      = 5
         , unitResists    = Set.fromList [ Fire, Ice ]
         , unitSource     = Set.fromList [ FromCity ]
         , unitAbilities  =
            [ produces (x,y) | x <- anyMana, y <- anyMana ] ++
            [ produces (5 *** Attack Melee ColdFire)
            , produces (5 *** Block ColdFire)
            , Black --> ChangeAttacks (AttackSetElement ColdFire)
            , Black --> ChangeAttacks (AttackSetType Siege)
            ]

         }

  , replicate 2 $
    Unit { unitName       = "Amotep Freezers"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 6
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [ produces (5 *** Attack Melee Physycal)
                            , Blue --> ChangeEnemy One
                                        (EnemyIf [ EnemyResists Ice False ]
                                           $ EnemyAnd 
                                              [ LooseArmor 3 1, NoAttack ])
                            ]
         }

  , replicate 2 $
    Unit { unitName       = "Amotep Gunners"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 6
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [ produces (5 *** Attack Melee Physycal)
                            , produces (5 *** Block Physycal)
                            , Red --> 6 *** Attack Ranged Fire
                            ]
         }

  , replicate 3 $
    Unit { unitName       = "Catapults"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [ produces [ 3 *** Attack Siege Physycal ]
                            , Red --> 5 *** Attack Siege Fire
                            , Blue --> 5 *** Attack Siege Ice ]
         }

  , replicate 2 $
    Unit { unitName       = "Fire Golems"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Fire, Physycal ]
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  = [ produces (3 *** Attack Melee Physycal)
                            , produces (3 *** Block Physycal)
                            , Red --> 4 *** Attack Ranged Fire ]
         }

  , replicate 2 $
    Unit { unitName       = "Fire Mages"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Fire ]
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = [ produces (3 *** Attack Ranged Fire)
                            , Red --> 6 *** Attack Melee Fire
                            , Red --> 6 *** Block Fire
                            , produces (Red, ManaCrystal Red)
                            ]
         }

  , replicate 2 $
    Unit { unitName       = "Ice Golems"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Ice, Physycal ]
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  = [ produces (3 *** Attack Melee Physycal)
                            , produces (3 *** Block Physycal)
                            , Blue --> 6 *** Attack Melee Ice ]
         }

  , replicate 2 $
    Unit { unitName       = "Ice Mages"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Ice ]
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = [ produces (4 *** Attack Melee Ice)
                            , produces (4 *** Block Ice)
                            , Blue --> 4 *** Attack Siege Ice
                            , produces (Blue, ManaCrystal Blue)
                            ]
         }
  ]




