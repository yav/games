{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
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
  ) where

import Common
import Rule


data Deed     = Deed { deedName      :: DeedName
                     , deedNamePower :: Maybe DeedName -- ^ For spells
                     , deedType      :: DeedType
                     , deedBasic     :: [ Rule ]
                     , deedPower     :: [ Rule ]
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
             , deedBasic     = []
             , deedPower     = []
             }

-- | Make a basic action.
actionDeed :: BasicMana -> DeedName -> [Rule] -> [Rule] -> Deed
actionDeed color name basic power =
  Deed { deedName      = name
       , deedNamePower = Nothing
       , deedType      = Action color
       , deedBasic     = basic
       , deedPower     = map (requires color &&&) power
       }

-- | Make an advanced action.
advancedActionDeed :: BasicMana -> DeedName -> [Rule] -> [Rule] -> Deed
advancedActionDeed color name basic power =
  Deed { deedName      = name
       , deedNamePower = Nothing
       , deedType      = AdvancedAction color
       , deedBasic     = basic
       , deedPower     = map (requires color &&&) power
       }

-- | Make a spell.
spellDeed :: BasicMana -> DeedName -> [Rule] -> DeedName -> [Rule] -> Deed
spellDeed color name basic powerName power =
  Deed { deedNamePower = Just powerName
       , deedName      = name
       , deedType      = Spell color
       , deedBasic     = map (requires color &&&) basic
       , deedPower     = map (requires (color,Black) &&&) power
       }

-- | Make an artifact.
artifactDeed :: DeedName -> [Rule] -> [Rule] -> Deed -- XXX
artifactDeed deedName deedBasic deedPower =
  Deed { deedNamePower = Nothing
       , deedType      = Artifact
       , ..
       }

