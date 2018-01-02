{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Deed
  ( Deed (..)
  , DeedType(..)
  , deedColor
  , DeedName
  , wound
  , actionDeed
  , advancedActionDeed
  , spellDeed
  , artifactDeed

  , combatAction
  ) where

import Control.Monad(unless)

import Common
import Act
import Player


data Deed     = Deed { deedName      :: DeedName
                     , deedNamePower :: Maybe DeedName -- ^ For spells
                     , deedType      :: DeedType
                     , deedBasic     :: Act ()
                     , deedPower     :: Act ()
                     }

deedColor :: Deed -> Maybe BasicMana
deedColor d =
  case deedType d of
    Action b         -> Just b
    AdvancedAction b -> Just b
    Spell b          -> Just b
    _                -> Nothing

data DeedType = Wound | Action BasicMana | AdvancedAction BasicMana
              | Spell BasicMana | Artifact

instance Eq Deed where
  x == y = deedName x == deedName y

instance Ord Deed where
  compare x y = compare (deedName x) (deedName y)


-- | Make a wound.
wound :: Deed
wound = Deed { deedName      = "Wound"
             , deedNamePower = Nothing
             , deedType      = Wound
             , deedBasic     = err
             , deedPower     = err
             }
  where err = reportError "Cannot use a wound card."

playable :: Deed -> Deed
playable c = it
  where
  it = c { deedBasic = deedBasic c >> cardPlayed it
         , deedPower = deedPower c >> cardPlayed it
         }

-- | Make a basic action.
actionDeed :: BasicMana -> DeedName -> Act () -> Act () -> Deed
actionDeed color name basic power =
  playable
  Deed { deedName      = name
       , deedNamePower = Nothing
       , deedType      = Action color
       , deedBasic     = basic
       , deedPower     = payMana 1 (BasicMana color) >> power
       }

-- | Make an advanced action.
advancedActionDeed :: BasicMana -> DeedName -> Act () -> Act () -> Deed
advancedActionDeed color name basic power =
  playable
  Deed { deedName      = name
       , deedNamePower = Nothing
       , deedType      = AdvancedAction color
       , deedBasic     = basic
       , deedPower     = payMana 1 (BasicMana color) >> power
       }

-- | Make a spell.
spellDeed :: BasicMana -> DeedName -> Act () -> DeedName -> Act () -> Deed
spellDeed color name basic powerName power =
  playable
  Deed { deedNamePower = Just powerName
       , deedName      = name
       , deedType      = Spell color
       , deedBasic     = payMana 1 (BasicMana color) >> basic
       , deedPower     =
          do t <- currentTime
             unless (t == Night) $
               reportError "Power spells can be cast only in the dark."
             payMana 1 Black
             payMana 1 (BasicMana color)
             power
       }

-- | Make an artifact.
artifactDeed :: DeedName -> Act () -> Act () -> Deed -- XXX
artifactDeed deedName deedBasic deedPower =
  playable
  Deed { deedNamePower = Nothing
       , deedType      = Artifact
       , ..
       }


combatAction :: [(CombatPhase,Act())] -> Act ()
combatAction opts =
  do c <- currentCombatPhase
     case lookup c opts of
       Just a -> a
       Nothing -> reportError
                        "This action does not work in the current combat phase."


