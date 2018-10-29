{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Market
  ( Market
  , emptyMarket
  -- * Queries
  , marketSingleEmpty
  , marketIsFull
  , marketConatains
  , marketAccepts
  , marketOwnerOf
  -- * Modifications
  , addTileMarket
  , rmTileMarket
  , completeMarket
  , vagrantJumpMarket
  , marketMoveSwap
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe (isNothing,isJust)
import Control.Monad(unless,guard)

import Config(winnerNum)
import Ids(PlayerId)
import Updater
import Tile



data Market       = Market { marketHas     :: Map Tile PlayerId
                           , marketMissing :: Set Tile
                           }


emptyMarket :: Market
emptyMarket =
  Market { marketHas = Map.empty
         , marketMissing = Set.fromList baseTiles
         }



marketSingleEmpty :: Market -> Maybe Tile
marketSingleEmpty Market{..} =
  do (a,rest) <- Set.minView marketMissing
     guard (Set.null rest)
     pure a

marketIsFull :: Market -> Bool
marketIsFull = isJust . marketSingleEmpty

marketAccepts :: Tile -> Market -> Bool
marketAccepts t m = not (marketIsFull m) && (t `Set.member` marketMissing m)

marketOwnerOf :: Tile -> Market -> Maybe PlayerId
marketOwnerOf t m = Map.lookup t (marketHas m)

marketConatains :: OwnedTile -> Market -> Bool
marketConatains OwnedTile { .. } m =
  case marketOwnerOf tileType m of
    Just pid -> pid == tileOwner
    Nothing  -> False


addTileMarket :: OwnedTile -> Updater Market ()
addTileMarket OwnedTile { .. } =
  do ok <- ask (marketAccepts tileType)
     unless ok (failure "The market has no space for this tile.")
     upd $ \Market{..} ->
            Market { marketHas     = Map.insert tileType tileOwner marketHas
                   , marketMissing = Set.delete tileType marketMissing
                   }

rmTileMarket :: OwnedTile -> Updater Market ()
rmTileMarket ot@OwnedTile {..} =
  do ok <- ask (marketConatains ot)
     unless ok (failure "The market does not have this tile.")
     upd $ \Market{..} ->
            Market { marketHas     = Map.delete tileType marketHas
                   , marketMissing = Set.insert tileType marketMissing
                   }


-- | Clear a full market and return the tiles that are to be
-- promoted, and the ones that get demoted.
completeMarket :: Updater Market ([OwnedTile],[OwnedTile])
completeMarket =
  do mb <- ask marketSingleEmpty
     case mb of
       Nothing -> failure "This market is not yet full."
       Just a ->
         do res <- ask (marketSplitAt a)
            set emptyMarket
            pure res
  where
  marketSplitAt a m =
    let (later,sooner) = Map.split a (marketHas m)
    in splitAt winnerNum
     $ map toOwnedTile
     $ Map.toList sooner ++ Map.toList later

  toOwnedTile (t,p) = OwnedTile { tileType = t, tileOwner = p }


-- | Jump to the front of the market queue.
-- Requirements:
--  * Market is full
--  * The new tile fits the empty space on the market
--  * The current last tile has the same owner as the new tile.
vagrantJumpMarket :: OwnedTile -> Updater Market ()
vagrantJumpMarket OwnedTile{..} =
  do full <- ask marketIsFull
     unless full (failure "This market is not full.")

     let t1 = nextTile tileType
         t2 = nextTile t1

     isEmpty <- ask (isNothing . marketOwnerOf t1)
     unless isEmpty (failure "Vagrant can jump only to the front of the line.")

     isLast <- ask (marketConatains OwnedTile { tileType = t2, .. })
     unless isLast (failure "Player needs to own last place.")

     upd $ \Market{..} ->
            Market { marketHas     = Map.insert t1 tileOwner marketHas
                   , marketMissing = Set.insert t2 marketMissing
                   }


-- | Add a new tile to the market, with the requirement that the market
-- becomes full afterwards.   If the new tile replaced an existing tile,
-- then return its owner.
marketMoveSwap :: OwnedTile -> Updater Market (Maybe PlayerId)
marketMoveSwap ot =
  do full <- ask marketIsFull
     if full then doSwap else doMove

  where
  t = tileType ot

  doSwap =
    do has <- ask marketHas
       case Map.lookup t has of
         Nothing  -> failure "This market is already full."
         Just pid ->
           do upd $ \m -> m { marketHas = Map.insert t (tileOwner ot) has }
              pure (Just pid)

  doMove =
    do addTileMarket ot
       full <- ask marketIsFull
       unless full (failure "Market must be full after move.")
       pure Nothing


