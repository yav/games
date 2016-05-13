module Hyperborea.Terrain where

import Util.Bag

data Terrain = Plain | Forest | Swamp | Mountain

extraCost :: Terrain -> (Int,Int)
extraCost t =
  case t of
    Plain     -> (0,0)
    Forest    -> (1,0)
    Swamp     -> (0,1)
    Mountain  -> (1,1)

moveCost :: Terrain -> Terrain -> Int
moveCost from to = exit + 1 + enter
  where
  (_,exit)  = extraCost from
  (enter,_) = extraCost to


data Location = Location
  { locTerrain  :: Terrain
  , locPlayers  :: Bag String   -- ^ In location, not in any features.
  , locFeautres :: [ Feature ]
  }


data Feature = Feature
  { featureType   :: FeatureType
  , featureGuests :: FeatureGuests
  }

data FeatureType = FeatureTypeXXX

data FeatureGuests =
    MultipleGuests (Bag String)
    -- ^ Supports any number of inhabitatnts.

  | SingleGuestOnce (Maybe String)
    -- ^ Supports at most one inhabitant.
    -- Cannot be used when occupied.

  | SingleGuestMany (Maybe String)
    -- ^ Supports at most one inhabitant.
    -- May be used when occupied---the previous guessed is "kicked-out".

