{-# LANGUAGE OverloadedStrings #-}
module AdvancedAction (deeds) where

import Control.Monad(unless,when)

import Common
import Deed
import Act

deeds :: [Deed]
deeds = blueDeeds ++ greenDeeds ++ redDeeds ++ whiteDeeds

blueDeeds :: [Deed]
blueDeeds =
  [ deed "Crystal Mastery"
      (do c <- chooseAvailableCrystal
          gainCrystal 1 c)
      (atEOT regainCrystals)

  , deed "Frost Bridge"
      (do gainMove 2
          reduceTerrainCostTo 1 Swamp)
      (do gainMove 4
          reduceTerrainCostTo 1 Lake
          reduceTerrainCostTo 1 Swamp)

  , deed "Ice Bolt"
      (gainCrystal 1 Blue)
      (gainAttack 3 Ranged Ice)

  , deed "Ice Shield"
      (gainBlock 3 Ice)
      (do gainBlock 3 Ice
          e <- currentlyBlocking
          reduceArmor 3 e )

  , let magic n = do c <- chooseBasicManaFrom anyBasicMana
                     case c of
                       Green -> gainMove n
                       White -> gainInfluence n
                       Blue  -> gainBlock n Physical
                       Red   -> gainAttack n Melee Physical
    in deed "Pure Magic" (magic 4) (magic 7)

  , let ask = do ans <- askText "Regain \"Steady Tempo\"?" ["Yes","No"]
                 return (ans == 0)
        self = deed "Steady Tempo"
                 (do gainMove 2
                     atEOT $ do empty <- currentDeedDeckEmpty
                                unless empty $
                                  do yes <- ask
                                     when yes $
                                       do removePlayed self
                                          addDeedDeckBottom self)
                (do gainMove 4
                    atEOT $ do yes <- ask
                               when yes (addDeedDeckTop self))
    in self

{-
  , deed "Magic Talent" [] [] -- XXX -}
  ]

  where
  deed = advancedActionDeed Blue

greenDeeds :: [Deed]
greenDeeds =
  [ {-deed "Crushing Bolt"
      [ produces (ManaCrystal Green) ]
      [ produces (3 *** Attack Siege Physycal) ]

  , deed "Refreshing Walk"
      [ produces (2 *** Movement) &&& produces Healing ]
      [ produces (4 *** Movement) &&& produces (2 *** Healing) ]

  , deed "Regeneration"
      [ produces Healing &&& produces (ReadyUnit l) | l <- [1,2] ]
      [ produces (2 *** Healing) &&& produces (ReadyUnit l) | l <- [1,2,3] ]

  , let terrain c t xs = produces (ChangeTerrainCost t c) &&& xs
        ts             = [ Hills, Forest, Wasteland, Desert, Swamp ]
    in deed "Path Finding"
        [ foldr (terrain (DecreaseBy 1 2)) (produces (2 *** Movement)) ts ]
        [ foldr (terrain (DecreaseTo 2))   (produces (4 *** Movement)) ts ]

{- XXX
  , deed "Ambush"
      [ produces (2 *** Movement) &&& produces Ambush ]
      [ produces (4 *** Movement) &&& produces PowerAmbush ]
-}

  , deed "In Need" [] [] -- XXX
  , deed "Training" [] [] -- XXX -}
  ]
  where
  deed = advancedActionDeed Green

redDeeds :: [Deed]
redDeeds =
  [ {- deed "Blood Rage"
      [ produces (2 *** Attack Melee Physycal)
      , produces (5 *** Attack Melee Physycal) &&& produces GainWound ]
      [ produces (4 *** Attack Melee Physycal)
      , produces (9 *** Attack Melee Physycal) &&& produces GainWound ]

  , deed "Blood Ritual"
      [ produces GainWound &&& produces (ManaToken (BasicMana Red))
          &&& produces (ManaToken t) | t <- anyMana ]
      [ produces GainWound &&& produces (ManaToken t1) &&&
        produces (ManaToken t2) &&& produces (ManaToken t3)
          | t1 <- anyMana, t2 <- anyMana, t3 <- anyMana ]

  , deed "Fire Bolt"
      [ produces (ManaCrystal Red) ]
      [ produces (3 *** Attack Ranged Fire) ]

  , deed "Intimidate"
      [ produces p &&& produces LooseReputation
          | p <- [ 4 *** Influence, 3 *** Attack Melee Physycal ] ]

      [ produces p &&& produces (2 *** LooseReputation)
          | p <- [ 8 *** Influence, 7 *** Attack Melee Physycal ] ]

{- XXX
  , deed "Decompose"
      [ DeedInHand n --> DeedDestroyed n &&& produces (2 *** ManaCrystal c)
          | a <- actions, let n = deedName a, n /= "Decompose"
          , Just c <- [ deedColor a ]
      ]
      [ DeedInHand n --> DeedDestroyed n :
                        [ ManaCrystal b | b <- anyBasicMana, b /= c ]
          | a <- actions, let n = deedName a, n /= "Decompose"
          , Just c <- [ deedColor a ]
      ]
-}

  , deed "Maximal Effect" [] []
  , deed "Into the Heat" [] [] -- XXX
  -}
  ]
  where
  deed = advancedActionDeed Red

-- XXX: white
whiteDeeds :: [Deed]
whiteDeeds =
  [ {- deed "Agility" [] []
  , deed "Diplomacy" [] []
  , deed "Heroic Tale" [] []
  , deed "Learning" [] []
  , deed "Mana Storm" [] []
  , deed "Song of Wind" [] []
  , deed "Swift Bolt" [] [] -}
  ]
  where
  deed = advancedActionDeed White
