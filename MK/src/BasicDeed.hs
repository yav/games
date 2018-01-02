{-# Language OverloadedStrings #-}
module BasicDeed where

import qualified Data.Set as Set

import Common
import Deed
import Act
import Player
import Enemies



blue :: [Deed]
blue =

  [ actionDeed Blue "Stamina" (gainMove 2) (gainMove 4)

  , actionDeed Blue "Determination"
      (combatAction
         [ (CombatBlocking, gainBlock 2 Physycal)
         , (CombatAttack, gainAttack 2 Melee Physycal)
         ])
      (gainBlock 5 Physycal)

  , actionDeed Blue "Crystallize"
      (do ms <- currentlyAvailableManaTypes
          m  <- case ms of
                  []  -> reportError "You do not have any mana."
                  [c] -> return c
                  _   -> chooseBasicManaFrom ms
          payMana 1 (BasicMana m)
          gainCrystal 1 m)

      (do m <- chooseBasicManaFrom anyBasicMana
          gainCrystal 1 m)

  ]



blueSpecial :: Deed
blueSpecial =
  actionDeed Blue "Cold Toughness"
    (combatAction
       [ (CombatBlocking, gainBlock 2 Ice)
       , (CombatAttack, gainAttack 2 Melee Ice)
       ])


    (do e <- currentlyBlocking
        let elementalAttack = case enemyAttack e of
                                Summoner -> error "Blocking a summoner"
                                AttacksWith el _ ->
                                  case el of
                                    Physycal -> 0
                                    Ice      -> 1
                                    Fire     -> 1
                                    ColdFire -> 2
            bonus = Set.size (enemyAbilities e) + elementalAttack
        gainBlock (5 + bonus) Ice
    )



green :: [Deed]
green =
  [ actionDeed Green "Concentration"
      (do m <- chooseBasicManaFrom [ Blue, White, Red ]
          gainMana 1 (BasicMana m))

      (concentrate 2)

  , actionDeed Green "March" (gainMove 2) (gainMove 4)

  , let opt x =
          do n <- askText "Draw cards or healing?" [ "Cards", "Healing" ]
             [ drawCard x, gainHeal x ] !! n
    in actionDeed Green "Tranquility" (opt 1) (opt 2)

  ]

greenSpecial :: Deed
greenSpecial =
  actionDeed Green "Will Focus"
    (do c <- chooseBasicManaFrom anyBasicMana
        case c of
          Green -> gainCrystal 1 Green
          _     -> gainMana 1 (BasicMana c))
    (concentrate 3)


red :: [Deed]
red =
  [ actionDeed Red "Improvisation" (improvise 3) (improvise 5)


  , actionDeed Red "Rage"
      (combatAction
        [ (CombatBlocking, gainBlock 2 Physycal)
        , (CombatAttack, gainAttack 2 Melee Physycal)
        ])
    (gainAttack 4 Melee Physycal)

  , actionDeed Red "Threaten"
      (gainInfluence 2)
      (gainInfluence 5 >> atEOT (looseReputation 1))

  ]


redSpecial :: Deed
redSpecial =
  actionDeed Red "Battle Versatility"
    (combatAction
       [ (CombatRangedAttack, gainAttack 1 Ranged Physycal)
       , (CombatBlocking, gainBlock 2 Physycal)
       , (CombatAttack, gainAttack 2 Melee Physycal)
       ])

    (combatAction
      [ (CombatRangedAttack,
          do as <- currentCombatEnemyEffects
             if Fortified `Set.member` as
                then gainAttack 2 Siege Physycal
                else gainAttack 3 Ranged Physycal)
        ,(CombatBlocking,
            do a <- currentlyBlocking
               case enemyAttack a of
                 AttacksWith Ice _ -> gainBlock 3 Fire
                 _ -> gainBlock 4 Physycal)
        ,(CombatAttack,
            do as <- currentCombatEnemyEffects
               if Resists Physycal `Set.member` as
                  then gainAttack 3 Melee Fire
                  else gainAttack 4 Melee Physycal)
        ])


white :: [Deed]
white =
  [ actionDeed White "Mana Draw" (gainManaDie 1)  (manaDraw 2)
  , actionDeed White "Promise"   (gainInfluence 2)(gainInfluence 4)
  , actionDeed White "Swiftness" (gainMove 2)     (gainAttack 3 Ranged Physycal)
  ]



whiteSpecial :: [Deed]
whiteSpecial =
  [ let also p = do c <- currentNormalTurnPhase
                    case c of
                      Interacting -> p
                      _ -> return ()
    in
    actionDeed White "Noble Manners"
      (do gainInfluence 2
          also (atEOT (gainFame 1)))
      (do gainInfluence 4
          also (atEOT (gainFame 1 >> gainReputation 1)))
  , actionDeed White "Mana Pull"
      (do c <- chooseSourceDie
          removeManaDie c
          gainUsedManaDie c
          d <- case c of
                 Gold ->
                   do t <- currentTime
                      case t of
                        Night -> reportError "Cannot use gold mana at night"
                        Day   -> BasicMana <$> chooseBasicManaFrom anyBasicMana
                 Black -> chooseManaFrom (Black : map BasicMana anyBasicMana)
                 BasicMana d -> return (BasicMana d)
          gainMana 1 d)

      (do manaDraw 1
          manaDraw 1
      )
  ]


concentrate :: Int -> Act ()
concentrate n =
  do c <- chooseCardFromHand
     -- Gain some mana to activate the advanced action
     case deedType c of
       Action col -> gainMana 1 (BasicMana col)
       AdvancedAction col -> gainMana 1 (BasicMana col)
       _ -> reportError "Card requires an action card."
     concentrated n (deedPower c)


improvise :: Int -> Act ()
improvise n =
  do c <- chooseCardFromHand
     discardCard c
     ph <- currentNormalTurnPhase
     case ph of
       Moving -> gainMove n
       Interacting -> gainInfluence n
       InCombat cph ->
         case cph of
           CombatBlocking -> gainBlock 3 Physycal
           CombatAttack   -> gainAttack 3 Melee Physycal
           _ -> reportError "This card cannot be played now."
       _ -> reportError "This card cannot be played now."


manaDraw :: Int -> Act ()
manaDraw n =
  do removeManaDie =<< chooseSourceDie
     d <- chooseManaFrom (Black : map BasicMana anyBasicMana)
     gainUsedManaDieFixed d
     gainMana n d






