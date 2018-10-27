module Tile (Tile, baseTiles, tileSet, nextTile, OwnedTile(..)) where

import Ids
import Config


newtype Tile      = Tile Int
                    deriving (Eq,Ord,Show)


tileSet :: [Tile]
tileSet = concatMap (replicate tileOfType) baseTiles

baseTiles :: [Tile]
baseTiles = [ Tile n | n <- take tileNum [ 0 .. ] ]


nextTile :: Tile -> Tile
nextTile (Tile x) = Tile (mod (x + 1) tileNum)

data OwnedTile    = OwnedTile { tileType :: Tile, tileOwner :: PlayerId }
                    deriving Eq
