module Hyperborea.Terrain where

import Utils.Bag

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
  , locPlayers  :: Bag String
  -- cities, ruins
  }



