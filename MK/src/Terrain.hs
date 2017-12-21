{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Static information about tiles, and valid tile placements.
module Terrain
  ( -- * Addressing
    Addr(..), TileAddr,

    HexAddr(..), allHexAddrs,
    Dir(..), allDirections,
    IsHexAddr(..),

    neighbour, neighboursUpTo, globalNeighbours,

    -- * Tile Description
    Tile(..),
    Terrain(..),
    Feature(..),
    TileType(..),
    HexLandInfo(..),
    terrainCostsDuring,

    -- * Tiles
    tileA, tileB, countryTiles, coreNonCityTiles, cityTiles,
    placeHolderTile,

    -- * Map Shape
    MapShape(..),
    openMap3, openMap4, openMap5,
    validPlacement
  ) where

import Common
import Enemies(EnemyType(Orc,Draconum))

import           Data.Array (array, (!))
import           Data.Text (Text)
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map

-- | Identifies a tile (7 hexes)
type TileAddr       = (Int,Int)

data Dir            = NE | E | SE | SW | W | NW
                      deriving (Eq,Ord,Show,Enum,Bounded)

-- | Identifies a hex on a tile.
data HexAddr        = Center | Border Dir
                      deriving (Eq,Ord,Show)

-- | Identifies a hex neighbouring another hex.
data HexNeighbour   = Local HexAddr       -- ^ Neighbor on the same tile
                    | Foreign Int Int Dir
                      -- ^ Neighbor on a different tile
                      -- The ints indicate the relative position of the other
                      -- tile.
                      deriving (Eq,Ord,Show)

-- | Identifies an individual hex on the board.
data Addr           = Addr { addrGlobal :: TileAddr
                             -- ^ Tile containing the hex.
                           , addrLocal :: HexAddr
                             -- ^ Location on the tile.
                           }
                      deriving (Eq,Ord,Show)

-- | Various features that may appear on a tile.
data Feature        = MagicalGlade | Mine BasicMana
                    | Village | Monastery
                    | Keep | MageTower
                    | Dungeon | Tomb
                    | MonsterDen | SpawningGrounds
                    | AncientRuins
                    | RampagingEnemy EnemyType
                      deriving (Eq,Show)

-- | Is this a basic or an advanced (core) tile.
data TileType       = BasicTile | CoreTile

-- | Description of a single hex.
data HexLandInfo    = HexLandInfo { hexTerrain :: Terrain
                                  , hexFeature :: Maybe Feature
                                  }

data Tile           = Tile { tileName     :: Text
                           , tileType     :: TileType
                           , tileTerrain  :: HexAddr -> HexLandInfo
                           }

-- | A dummy tile, used to indicate a potential tile location.
placeHolderTile :: TileType -> Tile
placeHolderTile ty = Tile { tileName    = "placeholder"
                          , tileType    = ty
                          , tileTerrain = \_ -> hexContent Ocean
                          }

-- | All possible directions.
allDirections :: [Dir]
allDirections = [ minBound .. maxBound ]

-- | All locations on a tile.
allHexAddrs :: [HexAddr]
allHexAddrs = Center : map Border allDirections

-- | Terrain costs.
terrainCostsDuring :: Time -> Map Terrain Int
terrainCostsDuring time = Map.fromList $
  [ (Plains, 2)
  , (Hills,  3)
  , (Forest, if time == Day then 3 else 5)
  , (Wasteland, 4)
  , (Desert, if time == Day then 5 else 3)
  , (Swamp, 5)
  ] ++
  [ (City m, 2) | m <- anyBasicMana ]



-- | Locations that are up to the given distance.
neighboursUpTo :: Int -> Addr -> Set Addr
neighboursUpTo n a = iterate step (Set.singleton a) !! n
  where
  neighbours x = Set.fromList [ neighbour x d | d <- allDirections ]
  step s       = Set.unions (map neighbours (Set.toList s))


-- | Location of hex one move in the given direction.
neighbour :: Addr -> Dir -> Addr
neighbour Addr { .. } dir =
  case tileGraph addrLocal dir of
    Local x         -> Addr { addrLocal = x, .. }
    Foreign dx dy a -> Addr { addrGlobal = ( fst addrGlobal + dx
                                           , snd addrGlobal + dy
                                           )
                            , addrLocal = Border a
                            }

-- | 'E' is in the direction of the increasing @x@ coordinate
globalDelta :: Dir -> TileAddr
globalDelta dir =
  case dir of
    E  -> (1,0)
    W  -> (-1,0)
    NE -> (0,1)
    NW -> (-1,1)
    SE -> (1,-1)
    SW -> (0,-1)

-- | Tile locaitons neighbouring some tile location.
globalNeighbours :: TileAddr -> [TileAddr]
globalNeighbours (x,y) =
  [ (x+dx,y+dy) | (dx,dy) <- map globalDelta allDirections ]


tileGraph :: HexAddr -> Dir -> HexNeighbour
tileGraph a d =
  case a of
    Center -> local d
    Border b ->
      case b of
        NW -> case d of
                NW -> Foreign 0 1 SW
                NE -> Foreign 0 1 SE
                E  -> local NE
                SE -> local Center
                SW -> local W
                W  -> Foreign (-1) 1 E
        NE -> case d of
                NW -> Foreign 0 1 SE
                NE -> Foreign 1 0 W
                E  -> Foreign 1 0 SW
                SE -> local E
                SW -> local Center
                W  -> local NW
        E -> case d of
               NW -> local NE
               NE -> Foreign 1 0 SW
               E  -> Foreign 1 (-1) NW
               SE -> Foreign 1 (-1) W
               SW -> local SE
               W  -> local Center
        SE -> case d of
                NW -> local Center
                NE -> local E
                E  -> Foreign 1 (-1) W
                SE -> Foreign 0 (-1) NE
                SW -> Foreign 0 (-1) NW
                W  -> local SW
        SW -> case d of
                NW -> local W
                NE -> local Center
                E  -> local SE
                SE -> Foreign 0 (-1) NW
                SW -> Foreign (-1) 0 E
                W  -> Foreign (-1) 0 NE
        W -> case d of
               NW -> Foreign (-1) 1 E
               NE -> local NW
               E  -> local Center
               SE -> local SW
               SW -> Foreign (-1) 0 NE
               W  -> Foreign (-1) 1 SE

  where local x = Local (hexAddr x)


--------------------------------------------------------------------------------

data MapShape = Wedge | OpenMap Int Int

openMap3 :: MapShape
openMap3 = OpenMap 1 (-1)

openMap4 :: MapShape
openMap4 = OpenMap 1 (-2)

openMap5 :: MapShape
openMap5 = OpenMap 2 (-2)

isOnMap :: MapShape -> TileAddr -> Bool
isOnMap sh (x,y) =
  case sh of
    Wedge -> x >= 0 && y >= 0
    OpenMap up down -> x >= 0 && y <= up && down <= y && (y >= 0 || -x <= y)

validPlacement ::
  MapShape           {- ^ Shapre of current map -} ->
  (TileAddr -> Bool) {- ^ Is the given address already explored -} ->
  TileType           {- ^ What sort of tile are we placing -} ->
  Bool               {- ^ Is this a backup tile? -} ->
  TileAddr           {- ^ Location where we'd like to place -} ->
  Bool               {- ^ Is this placement valid -}
validPlacement sh explored t backup pt@(x,y) =
  isOnMap sh pt && not (explored pt) &&
  (case (t, neighboursOf pt) of
     (BasicTile, _ : _ : xs) -> backupCheck xs
     (BasicTile, [b]) | not backup -> case neighboursOf b of
                                        _ : _ : _ -> True
                                        _         -> False
     (CoreTile, _ : _ : xs)  ->
       case sh of
          Wedge -> x /= 0 && y /= 0 && backupCheck xs
                   -- In wedge maps, core tiles can't be on the coast.
          _     -> backupCheck xs
     _                       -> False)
  where
  backupCheck xs  = not backup || not (null xs)
  neighboursOf a  = [ b | b <- globalNeighbours a, explored b ]



--------------------------------------------------------------------------------

class IsHexAddr a where
  hexAddr :: a -> HexAddr

instance IsHexAddr Dir where
  hexAddr = Border

instance IsHexAddr HexAddr where
  hexAddr = id



class IsHexContent a where
  hexContent :: a -> HexLandInfo

instance IsHexContent Terrain where
  hexContent a = HexLandInfo { hexTerrain = a, hexFeature = Nothing }

instance IsHexContent (Terrain,Feature) where
  hexContent (a,b) = HexLandInfo { hexTerrain = a, hexFeature = Just b }

instance IsHexContent (Terrain,EnemyType) where
  hexContent (a,b) = HexLandInfo { hexTerrain = a
                             , hexFeature = Just (RampagingEnemy b)
                             }

type Hex = (HexAddr, HexLandInfo)

listTileFun :: [Hex] -> HexAddr -> HexLandInfo
listTileFun xs = \d -> arr ! cvt d
  where
  arr   = array (0,1 + fromEnum (maxBound :: Dir)) [ (cvt c,a) | (c,a) <- xs ]
  cvt c = case c of
            Center   -> 0
            Border b -> 1 + fromEnum b

(|->) :: (IsHexAddr a, IsHexContent b) => a -> b -> Hex
a |-> b = (hexAddr a, hexContent b)

tile :: Text -> TileType -> [Hex] -> Tile
tile tileName tileType hexes = Tile { tileTerrain = listTileFun hexes, .. }


tileA :: Tile
tileA = tile "A" BasicTile [ NW     |-> Plains
                           , NE     |-> Forest
                           , W      |-> Ocean
                           , Center |-> Plains -- and portal...
                           , E      |-> Plains
                           , SW     |-> Ocean
                           , SE     |-> Ocean
                           ]

tileB :: Tile
tileB = tile "B" BasicTile [ NW     |-> Plains
                           , NE     |-> Forest
                           , W      |-> Ocean
                           , Center |-> Plains -- and portal...
                           , E      |-> Plains
                           , SW     |-> Ocean
                           , SE     |-> Plains
                           ]

countryTiles :: [Tile]
countryTiles = map country
  [ ("1", [ NW     |-> (Forest, Orc)
          , NE     |-> Lake
          , W      |-> Forest
          , Center |-> (Forest, MagicalGlade)
          , E      |-> (Plains, Village)
          , SW     |-> Plains
          , SE     |-> Plains
          ])

  , ("2", [ NW     |-> (Hills, Orc)
          , NE     |-> (Forest, MagicalGlade)
          , W      |-> Plains
          , Center |-> Hills
          , E      |-> (Plains, Village)
          , SW     |-> (Hills, Mine Green)
          , SE     |-> Plains
          ])

  , ("3", [ NW     |-> Plains
          , NE     |-> (Hills, Keep)
          , W      |-> Plains
          , Center |-> Forest
          , E      |-> Hills
          , SW     |-> (Plains, Village)
          , SE     |-> (Hills, Mine White)
          ])

  , ("4", [ NW     |-> Desert
          , NE     |-> Desert
          , W      |-> (Hills, Orc)
          , Center |-> (Desert, MageTower)
          , E      |-> Mountain
          , SW     |-> Plains
          , SE     |-> (Plains, Village)
          ])

  , ("5", [ NW     |-> Forest
          , NE     |-> (Plains, Monastery)
          , W      |-> (Forest, MagicalGlade)
          , Center |-> Lake
          , E      |-> (Plains, Orc)
          , SW     |-> Forest
          , SE     |-> (Hills, Mine Blue)
          ])

  , ("6", [ NW     |-> Mountain
          , NE     |-> Forest
          , W      |-> (Hills, MonsterDen)
          , Center |-> (Hills, Mine Red)
          , E      |-> Plains
          , SW     |-> Hills
          , SE     |-> (Forest, Orc)
          ])

  , ("7", [ NW     |-> Lake
          , NE     |-> (Forest, Orc)
          , W      |-> (Plains, Monastery)
          , Center |-> Swamp
          , E      |-> (Forest, MagicalGlade)
          , SW     |-> Plains
          , SE     |-> (Plains, Dungeon)
          ])

  , ("8", [ NW     |-> (Forest, MagicalGlade)
          , NE     |-> (Forest, AncientRuins)
          , W      |-> Forest
          , Center |-> (Swamp, Orc)
          , E      |-> Plains
          , SW     |-> Swamp
          , SE     |-> (Swamp, Village)
          ])

  , ("9", [ NW     |-> (Wasteland, Dungeon)
          , NE     |-> Mountain
          , W      |-> Plains
          , Center |-> Mountain
          , E      |-> (Wasteland, Keep)
          , SW     |-> (Wasteland, MageTower)
          , SE     |-> Plains
          ])

  , ("10",[ NW     |-> (Hills, MonsterDen)
          , NE     |-> Forest
          , W      |-> Hills
          , Center |-> Mountain
          , E      |-> Plains
          , SW     |-> (Hills, Keep)
          , SE     |-> (Hills, AncientRuins)
          ])

  , ("11",[ NW     |-> Hills
          , NE     |-> Lake
          , W      |-> (Plains, AncientRuins)
          , Center |-> (Plains, MageTower)
          , E      |-> Lake
          , SW     |-> Lake
          , SE     |-> (Hills, Orc)
          ])
  ]
  where
  country (x,y) = tile x BasicTile y



coreNonCityTiles :: [Tile]
coreNonCityTiles = map core
  [ ("1", [ NW     |-> Mountain
          , NE     |-> (Desert, Tomb)
          , W      |-> (Hills, SpawningGrounds)
          , Center |-> (Desert, Monastery)
          , E      |-> Desert
          , SW     |-> Hills
          , SE     |-> Desert
          ])

  , ("2", [ NW     |-> Lake
          , NE     |-> (Swamp, AncientRuins)
          , W      |-> Forest
          , Center |-> Lake
          , E      |-> (Hills, Mine Green)
          , SW     |-> (Swamp, MageTower)
          , SE     |-> (Swamp, Draconum)
          ])

  , ("3", [ NW     |-> Mountain
          , NE     |-> (Wasteland, AncientRuins)
          , W      |-> (Wasteland, Tomb)
          , Center |-> Wasteland
          , E      |-> (Hills, MageTower)
          , SW     |-> (Hills, Mine White)
          , SE     |-> Wasteland
          ])

  , ("4", [ NW     |-> (Hills, Mine Blue)
          , NE     |-> Hills
          , W      |-> Wasteland
          , Center |-> (Mountain, Draconum)
          , E      |-> (Wasteland, Keep)
          , SW     |-> (Wasteland, AncientRuins)
          , SE     |-> Wasteland
          ])
  ]
  where
  core (x,y) = tile x CoreTile y


cityTiles :: [Tile]
cityTiles = map core
  [ ("5", [ NW     |-> (Forest, MagicalGlade)
          , NE     |-> (Swamp, Village)
          , W      |-> Lake
          , Center |-> City Green
          , E      |-> (Swamp, Orc)
          , SW     |-> (Forest, Orc)
          , SE     |-> Swamp
          ])

  , ("6", [ NW     |-> Forest
          , NE     |-> (Plains, Monastery)
          , W      |-> (Mountain, Draconum)
          , Center |-> City Blue
          , E      |-> Lake
          , SW     |-> Hills
          , SE     |-> Lake
          ])

  , ("7", [ NW     |-> (Wasteland, SpawningGrounds)
          , NE     |-> Plains
          , W      |-> (Wasteland, Keep)
          , Center |-> City White
          , E      |-> Forest
          , SW     |-> Lake
          , SE     |-> (Lake, Draconum)
          ])

  , ("8", [ NW     |-> (Desert, AncientRuins)
          , NE     |-> (Hills, Mine Red)
          , W      |-> (Wasteland, Draconum)
          , Center |-> City Red
          , E      |-> Desert
          , SW     |-> Wasteland
          , SE     |-> (Desert, Draconum)
          ])

  ]
  where
  core (x,y) = tile x CoreTile y



