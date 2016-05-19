{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Hyperborea.Terrain where

import Util.Perhaps
import Util.Bag
import Data.Text(Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad(msum,guard)
import Data.Maybe(mapMaybe)

type PlayerId = Text
type Area     = Map Addr Location
type Addr     = (Int,Int)

data Dir = NE | E | SE | SW | W | NW
            deriving (Bounded,Enum)

addrInDir :: Dir -> Addr -> Addr
addrInDir d (x,y) =
  case d of
    E  -> (x + 1, y)
    W  -> (x - 1, y)
    NE -> (x, y + 1)
    SW -> (x, y - 1)
    SE -> (x + 1, y - 1)
    NW -> (x - 1, y + 1)

adjacent :: Addr -> Addr -> Bool
adjacent x y = or [ addrInDir d x == y | d <- [ minBound .. maxBound ] ]

revealNeighbours :: Addr -> Area -> Area
revealNeighbours a area = foldr updInDir area [ minBound .. maxBound ]
  where
  updInDir d = Map.adjust upd (addrInDir d a)
  upd l      = l { locRevealed = True }

emptyArea :: Area
emptyArea = Map.empty

moveAvatar :: PlayerId -> Addr -> Addr -> Area -> Perhaps (Int,Area)
moveAvatar a from to area =
  do checkThat (adjacent from to) "Avatars can move just one space at a time."
     lFrom <- exitLocation a  =<< getLoc from
     lTo   <- enterLocation a =<< getLoc to
     let cost = moveCost (locTerrain lFrom) (locTerrain lTo)
     return ( cost
            , revealNeighbours to $ Map.insert from lFrom
                                  $ Map.insert to lTo area )

  where getLoc addr = perhaps "Address not map" (Map.lookup addr area)





data Terrain = Plain | Forest | Swamp | Mountain
                deriving Eq

extraCost :: Terrain -> (Int,Int)
extraCost t =
  case t of
    Plain     -> (0,0)
    Forest    -> (1,0)
    Swamp     -> (0,1)
    Mountain  -> (1,1)

moveCost :: Terrain -> Terrain -> Int
moveCost from to = if from == to then 1 else exit + 1 + enter
  where
  (_,exit)  = extraCost from
  (enter,_) = extraCost to





data Location = Location
  { locTerrain  :: Terrain
  , locRevealed :: Bool
  , locAvatars  :: Bag PlayerId   -- ^ In location, not in any features.
  , locFeautres :: Map Int Feature -- ^ Feature id |-> Feature
  }


data Feature = Feature
  { featureType   :: FeatureType
  , featureGuests :: Maybe PlayerId
  }

data FeatureType = FeatureTypeXXX

-- | Find a feature by id.
getFeature :: Int -> Location -> Perhaps Feature
getFeature fid l =
  perhaps "This location does not exist." (Map.lookup fid (locFeautres l))

checkRevealed :: Location -> Perhaps ()
checkRevealed l =
  checkThat (locRevealed l) "This location is not yet available."


-- | Add an avatar to a location.
enterLocation :: PlayerId -> Location -> Perhaps Location
enterLocation a l =
  do checkRevealed l
     return l { locAvatars = bagAdd 1 a (locAvatars l) }

-- | Remove an avatar from a location.
exitLocation :: PlayerId -> Location -> Perhaps Location
exitLocation a l =
  do as <- perhaps "This player has no available avatars."
             (bagRemove 1 a (locAvatars l))
     return l { locAvatars = as }

-- | Move an avatar from the the general area to a specific location.
enterFeature :: PlayerId -> Int -> Location -> Perhaps Location
enterFeature a fid l =
  do f <- getFeature fid l
     case featureGuests f of
       Nothing ->
         do l1 <- exitLocation a l
            let f1 = f { featureGuests = Just a }
            return l1 { locFeautres = Map.insert fid f1 (locFeautres l1) }
       Just _  -> Failed "This location is already occupied."

-- | Move an avatar from a location to the general area.
exitFeature :: Int -> Location -> Perhaps Location
exitFeature fid l =
  do f <- getFeature fid l
     case featureGuests f of
       Nothing -> Failed "This location is empty."
       Just b  ->
         do l1 <- enterLocation b l
            let f1 = f { featureGuests = Nothing }
            return l1 { locFeautres = Map.insert fid f1 (locFeautres l1) }

-- | Move all avatar for a player to the general area
resetLocation :: PlayerId -> Location -> Location
resetLocation pid l =
  l { locAvatars  = bagAdd (sum as) pid (locAvatars l)
    , locFeautres = foldr (uncurry Map.insert) (locFeautres l) changed
    }
  where
  (as, changed) = unzip $ mapMaybe upd $ Map.toList $ locFeautres l

  upd (x,f) = do a <- featureGuests f
                 guard (a == pid)
                 return (1, (x, f { featureGuests = Nothing }))

-- | Remove an avatar either from the general area or from a feature.
killAvatar :: PlayerId -> Location -> Perhaps Location
killAvatar a l =
  case bagRemove 1 a (locAvatars l) of
    Just as -> return l { locAvatars = as }
    Nothing ->
      do (k,v) <- perhaps "No one to target."
                    $ msum $ map upd $ Map.toList $ locFeautres l
         return l { locFeautres = Map.insert k v (locFeautres l) }
  where
  upd (x,f) = case featureGuests f of
                Just b | a == b -> return (x,f { featureGuests = Nothing })
                _               -> Nothing

