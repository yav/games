{-# Language OverloadedStrings #-}
module Component where

import Data.Text(Text)
import Util.Bag

import BasicTypes
import Language


data Component =
  Component
  { name              :: Text
    -- ^ Name of the component

  , componentType     :: ComponentType
    -- ^ What sort of component is this

  , isLifeForm        :: [LifeForm]
    -- ^ Markers for special artifcats

  , cost              :: [Cost]
    -- ^ What needs to be paid to build this component

  , points            :: Points
    -- ^ How this awards points

  , holding           :: Bag Resource
    -- ^ Resources stored on this component

  , collect           :: [Steps]

    -- powers, abilities, reactions
  , powers            :: [Steps]
  }

data Cost   = PayAny Int | Pay Int Resource

data Points = FixedPoints Int
            | PointsPerResource Resource Int
            | PointsPerLifeFrom LifeForm Int
            | PointsAnd Points Points

blankComponent :: ComponentType -> Component
blankComponent t =
  Component
    { name = ""
    , componentType = t
    , isLifeForm = []
    , cost = []
    , points = FixedPoints 0
    , holding = bagEmpty
    , collect = []
    , powers = []
    }


