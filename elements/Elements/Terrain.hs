{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Elements.Terrain
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
    EnemyGroup(..),
    EnemyVis(..),
    TileType(..),
    HexLandInfo(..),
    terrainCosts,

    -- * Tiles
    tileA, tileB, basicTiles, advancedTiles,
    placeHolderTile,

    -- * Map Shape
    MapShape(..),
    openMap3, openMap4, openMap5,
    validPlacement
  ) where

import           Data.Array (array, (!))
import           Data.Text (Text)
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Text as Text

import Elements.BasicTypes
import Elements.Enemies



type TileAddr       = (Int,Int)

data Dir            = NE | E | SE | SW | W | NW
                      deriving (Eq,Ord,Show,Enum,Bounded)

data HexAddr        = Center | Border Dir
                      deriving (Eq,Ord,Show)

data HexNeighbour   = Local HexAddr | Foreign Int Int Dir
                      deriving (Eq,Ord,Show)

data Addr           = Addr { addrGlobal :: TileAddr, addrLocal :: HexAddr }
                      deriving (Eq,Ord,Show)

data Terrain        = Road | Hills | Forest | Lava | Desert | Bog
                    | Sea | Mountain
                    | Empty
                      deriving (Eq,Ord,Show)

data Feature        = Shop | Castle | Rest Spirit
                      deriving (Eq,Show)

data EnemyVis       = EnemyVisible  -- ^ Starts visible
                    | EnemyHidden   -- ^ Revealed on appraoch
                    | EnemyAmbush   -- ^ Reveal on attack
                      deriving (Eq,Ord)
                      -- If a bigger visibility is revealed, all smaller ones
                      -- are revealed too.

data EnemyGroup     = EnemyGroup { egType :: EnemyType
                                 , egSize :: Int
                                 , egVis  :: EnemyVis
                                 }

data TileType       = BasicTile | AdvancedTile
                      deriving Eq

data HexLandInfo    = HexLandInfo { hexTerrain      :: Terrain
                                  , hexFeatures     :: [ Feature ]
                                  , hexSpawnEnemies :: [ EnemyGroup ]
                                  }

data Tile           = Tile { tileName     :: Text
                           , tileType     :: TileType
                           , tileTerrain  :: HexAddr -> HexLandInfo
                           }

placeHolderTile :: TileType -> Tile
placeHolderTile ty = Tile { tileName    = "placeholder"
                          , tileType    = ty
                          , tileTerrain = \_ -> hexContent Empty
                          }

allDirections :: [Dir]
allDirections = [ minBound .. maxBound ]

allHexAddrs :: [HexAddr]
allHexAddrs = Center : map Border allDirections


terrainCosts :: Map Terrain Int
terrainCosts = Map.fromList $
  [ (Road,    2)
  , (Hills,   3)
  , (Forest,  3)
  , (Lava,    4)
  , (Desert,  5)
  , (Bog,     5)
  ]



-- | Locations that are up to the given distance.
neighboursUpTo :: Int -> Addr -> Set Addr
neighboursUpTo n a = iterate step (Set.singleton a) !! n
  where
  neighbours x = Set.fromList [ neighbour x d | d <- allDirections ]
  step s       = Set.unions (map neighbours (Set.toList s))
  -- We don't need to add the original locations: they will be added
  -- anyway because they are neighbours of the new neighbours.


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

validPlacement :: MapShape -> (TileAddr -> Bool) -> TileType -> Bool ->
                  TileAddr -> Bool
validPlacement sh explored t backup pt@(x,y) =
  isOnMap sh pt && not (explored pt) &&
  (case (t, neighboursOf pt) of
     (BasicTile, _ : _ : xs) -> backupCheck xs
     (BasicTile, [b]) | not backup -> case neighboursOf b of
                                        _ : _ : _ -> True
                                        _         -> False
     (AdvancedTile, _ : _ : xs)  ->
       case sh of
          Wedge -> x /= 0 && y /= 0 && backupCheck xs
                   -- In wedge maps, core tiles can't be on the coast.
          _     -> backupCheck xs
     _                       -> False)
  where
  backupCheck xs  = not backup || not (null xs)
  neighboursOf a  = [ b | b <- globalNeighbours a, explored b ]





--------------------------------------------------------------------------------
-- DSL for specifying the tiles


class IsHexAddr a where
  hexAddr :: a -> HexAddr

instance IsHexAddr Dir where
  hexAddr = Border

instance IsHexAddr HexAddr where
  hexAddr = id

class IsFeature t where
  toFeature :: t -> Feature

instance IsFeature Feature where
  toFeature = id

instance IsFeature Spirit where
  toFeature = Rest

instance IsFeature Element where
  toFeature = toFeature . Element


class IsHexContent a where
  hexContent :: a -> HexLandInfo

instance IsHexContent Terrain where
  hexContent a = HexLandInfo { hexTerrain       = a
                             , hexFeatures      = []
                             , hexSpawnEnemies  = [] }

instance IsFeature f => IsHexContent (Terrain,f) where
  hexContent (a,b) = HexLandInfo { hexTerrain       = a
                                 , hexFeatures      = [ toFeature b ]
                                 , hexSpawnEnemies  = []
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

--------------------------------------------------------------------------------
-- The tiles


tileA :: Tile
tileA = tile "A" BasicTile [ NW     |-> Road
                           , NE     |-> Forest
                           , W      |-> Empty
                           , Center |-> Road
                           , E      |-> Road
                           , SW     |-> Empty
                           , SE     |-> Empty
                           ]

tileB :: Tile
tileB = tile "B" BasicTile [ NW     |-> Road
                           , NE     |-> Forest
                           , W      |-> Empty
                           , Center |-> Road
                           , E      |-> Road
                           , SW     |-> Empty
                           , SE     |-> Road
                           ]

basicTiles :: [Tile]
basicTiles = map basic
  [ ("1", [ NW     |-> (Forest, Castle)
          , NE     |-> Sea
          , W      |-> Forest
          , Center |-> (Forest, Birth)
          , E      |-> (Road, Shop)
          , SW     |-> Road
          , SE     |-> Road
          ])

  , ("2", [ NW     |-> (Hills, Castle)
          , NE     |-> (Forest, Birth)
          , W      |-> Road
          , Center |-> Hills
          , E      |-> (Road, Shop)
          , SW     |-> (Hills, Earth)
          , SE     |-> Road
          ])

  , ("3", [ NW     |-> Road
          , NE     |-> (Hills, Shop)
          , W      |-> Road
          , Center |-> Forest
          , E      |-> Hills
          , SW     |-> (Road, Shop)
          , SE     |-> (Hills, Air)
          ])

  , ("4", [ NW     |-> Desert
          , NE     |-> Desert
          , W      |-> (Hills, Castle)
          , Center |-> (Desert, Castle)
          , E      |-> Mountain
          , SW     |-> Road
          , SE     |-> (Road, Shop)
          ])

  , ("5", [ NW     |-> Forest
          , NE     |-> (Road, Shop)
          , W      |-> (Forest, Birth)
          , Center |-> Sea
          , E      |-> (Road, Castle)
          , SW     |-> Forest
          , SE     |-> (Hills, Water)
          ])

  , ("6", [ NW     |-> Mountain
          , NE     |-> Forest
          , W      |-> (Hills, Castle)
          , Center |-> (Hills, Fire)
          , E      |-> Road
          , SW     |-> Hills
          , SE     |-> (Forest, Castle)
          ])

  , ("7", [ NW     |-> Sea
          , NE     |-> (Forest, Castle)
          , W      |-> (Road, Shop)
          , Center |-> Bog
          , E      |-> (Forest, Birth)
          , SW     |-> Road
          , SE     |-> (Road, Castle)
          ])

  , ("8", [ NW     |-> (Forest, Birth)
          , NE     |-> (Forest, Castle)
          , W      |-> Forest
          , Center |-> (Bog, Castle)
          , E      |-> Road
          , SW     |-> Bog
          , SE     |-> (Bog, Shop)
          ])

  , ("9", [ NW     |-> (Lava, Castle)
          , NE     |-> Mountain
          , W      |-> Road
          , Center |-> Mountain
          , E      |-> (Lava, Shop)
          , SW     |-> (Lava, Castle)
          , SE     |-> Road
          ])

  , ("10",[ NW     |-> (Hills, Castle)
          , NE     |-> Forest
          , W      |-> Hills
          , Center |-> Mountain
          , E      |-> Road
          , SW     |-> (Hills, Shop)
          , SE     |-> (Hills, Castle)
          ])

  , ("11",[ NW     |-> Hills
          , NE     |-> Sea
          , W      |-> (Road, Castle)
          , Center |-> (Road, Castle)
          , E      |-> Sea
          , SW     |-> Sea
          , SE     |-> (Hills, Castle)
          ])
  ]
  where
  basic (x,y) = tile (Text.cons 'b' x) BasicTile y



advancedTiles :: [Tile]
advancedTiles = map advanced
  [ ("1", [ NW     |-> Mountain
          , NE     |-> (Desert, Castle)
          , W      |-> (Hills, Castle)
          , Center |-> (Desert, Shop)
          , E      |-> Desert
          , SW     |-> Hills
          , SE     |-> Desert
          ])

  , ("2", [ NW     |-> Sea
          , NE     |-> (Bog, Castle)
          , W      |-> Forest
          , Center |-> Sea
          , E      |-> (Hills, Earth)
          , SW     |-> (Bog, Castle)
          , SE     |-> (Bog, Castle)
          ])

  , ("3", [ NW     |-> Mountain
          , NE     |-> (Lava, Castle)
          , W      |-> (Lava, Castle)
          , Center |-> Lava
          , E      |-> (Hills, Castle)
          , SW     |-> (Hills, Air)
          , SE     |-> Lava
          ])

  , ("4", [ NW     |-> (Hills, Water)
          , NE     |-> Hills
          , W      |-> Lava
          , Center |-> (Mountain, Castle)
          , E      |-> (Lava, Shop)
          , SW     |-> (Lava, Castle)
          , SE     |-> Lava
          ])

  , ("5", [ NW     |-> (Forest, Birth)
          , NE     |-> (Bog, Shop)
          , W      |-> Sea
          , Center |-> Bog
          , E      |-> (Bog, Castle)
          , SW     |-> (Forest, Castle)
          , SE     |-> Bog
          ])

  , ("6", [ NW     |-> Forest
          , NE     |-> (Road, Shop)
          , W      |-> (Mountain, Castle)
          , Center |-> Road
          , E      |-> Sea
          , SW     |-> Hills
          , SE     |-> Sea
          ])

  , ("7", [ NW     |-> (Lava, Castle)
          , NE     |-> Road
          , W      |-> (Lava, Shop)
          , Center |-> Road
          , E      |-> Forest
          , SW     |-> Sea
          , SE     |-> (Sea, Castle)
          ])

  , ("8", [ NW     |-> (Desert, Castle)
          , NE     |-> (Hills, Fire)
          , W      |-> (Lava, Castle)
          , Center |-> Desert
          , E      |-> Desert
          , SW     |-> Lava
          , SE     |-> (Desert, Castle)
          ])

  ]
  where
  advanced (x,y) = tile (Text.cons 'a' x) AdvancedTile y



