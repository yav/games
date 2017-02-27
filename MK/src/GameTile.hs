{-# LANGUAGE Safe, OverloadedStrings, RecordWildCards #-}
-- | An active game tile.
module GameTile
  ( GameTile
  , gameTilePlaceHolder
  , emptyGameTile
  , HexInfo(..)
  , gameTileInfo
  , gameTileUpdateAt
  , gameTileUpdateAt'
  , gameTileSearch
  , gameTileIsSafe
  ) where

import Common
import Terrain
import HexContent

import           Data.Map ( Map )
import qualified Data.Map as Map


-- | An active game tile.  Keeps track of what's on each hex.
data GameTile = GameTile
  { gameTile        :: Tile
  , gameTileContent :: Map HexAddr HexContent
    -- ^ A missing entry, and `emptyHexContent` should be equivalent.
  }

-- | A place-holder tile.  This is used to indicate where we can explore next.
gameTilePlaceHolder :: TileType -> GameTile
gameTilePlaceHolder t = GameTile { gameTileContent = Map.empty
                                 , gameTile        = placeHolderTile t
                                 }

emptyGameTile :: Tile -> GameTile
emptyGameTile gameTile = GameTile { gameTileContent = Map.empty, .. }


-- | Information about a single hex on an active tile.
data HexInfo = HexInfo
  { hexLandInfo :: HexLandInfo
  , hexContent  :: HexContent     -- ^ Dynamic information
  }

-- | Information about the content of a specific tile.
gameTileInfo :: HexAddr -> GameTile -> HexInfo
gameTileInfo a GameTile { .. } =
  HexInfo { hexContent  = Map.findWithDefault hexEmpty a gameTileContent
          , hexLandInfo = tileTerrain gameTile a
          }

-- | Update the content of a hex.
gameTileUpdateAt :: HexAddr -> (HexInfo -> HexContent) -> GameTile -> GameTile
gameTileUpdateAt a f gt =
  gt { gameTileContent = Map.insert a (f (gameTileInfo a gt))
                                      (gameTileContent gt) }

-- | Update the content of a hex.
gameTileUpdateAt' :: HexAddr -> (HexInfo -> (a,HexContent))
                             -> GameTile -> (a, GameTile)
gameTileUpdateAt' a f gt =
  (res, gt { gameTileContent = Map.insert a content (gameTileContent gt) })
  where (res,content) = f (gameTileInfo a gt)



-- | Find addresses on the tile satisfying a predicate.
gameTileSearch :: (HexInfo -> Bool) -> GameTile -> [ HexAddr ]
gameTileSearch p GameTile { .. } =
  [ a | (a, hexContent) <- Map.toList gameTileContent
      , p HexInfo { hexLandInfo = tileTerrain gameTile a, .. }
      ]


-- | Is this a safe location for the given player.
gameTileIsSafe :: GameTile -> HexAddr -> PlayerId -> Bool
gameTileIsSafe gt loc p =
  case hexTerrain of
    City _    -> noEnemies
    Lake      -> False
    Mountain  -> False
    Ocean     -> False
    _ -> case hexFeature of
           Nothing -> True
           Just f  -> not (hexHasPlayers hexContent) &&
             (case f of
                Keep             -> hexHasShield p hexContent
                MageTower        -> noEnemies
                RampagingEnemy _ -> noEnemies
                _                -> True)
  where
  HexInfo { hexLandInfo = HexLandInfo { .. },  .. } = gameTileInfo loc gt
  noEnemies      = not (hexHasEnemies hexContent)

