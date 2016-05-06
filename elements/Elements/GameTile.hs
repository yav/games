{-# LANGUAGE Safe, OverloadedStrings, RecordWildCards #-}
-- | An active game tile.
module Elements.GameTile
  ( GameTile
  , gameTilePlaceHolder
  , emptyGameTile
  , HexInfo(..)
  , HexContent(..)
  , hexReveal
  , gameTileInfo
  , gameTileUpdateAt
  , gameTileUpdateAt'
  , gameTileSearch
  ) where

import Elements.Terrain
import Elements.Enemies

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Text ( Text )

data HexContent = HexContent
  { hexEnemies  :: [(EnemyVis,Enemy)]
  }

emptyHexContent :: HexContent
emptyHexContent = HexContent { hexEnemies = [] }

hexReveal :: EnemyVis -> HexContent -> HexContent
hexReveal from HexContent { .. } =
  HexContent { hexEnemies = map reveal hexEnemies, .. }
  where reveal (x,e) = (if x <= from then EnemyVisible else x, e)




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
  HexInfo { hexContent  = Map.findWithDefault emptyHexContent a gameTileContent
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





--------------------------------------------------------------------------------


