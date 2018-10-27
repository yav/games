{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Market where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe (isNothing)
import Control.Monad(unless)

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

marketIsFull :: Market -> Bool
marketIsFull m = Set.size (marketMissing m) == 1

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


completeMarket :: Updater Market ([OwnedTile],[OwnedTile])
completeMarket =
  do Market{..} <- get
     case Set.minView marketMissing of
       Just (a,bs)
         | Set.null bs ->
           do set emptyMarket
              pure $ splitAt winnerNum
                   $ map toOwnedTile
                   $ Map.toList sooner ++ Map.toList later
         where
         (later,sooner)    = Map.split a marketHas
         toOwnedTile (t,p) = OwnedTile { tileType = t, tileOwner = p }

       _ -> failure "Market is not yet full."


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


