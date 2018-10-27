{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Area where

import Data.Map(Map)
import qualified Data.Map as Map

import Util.Perhaps

import Updater
import Ids
import Tile
import Market



data Area         = Area { areaMarkets :: Map MarketId Market
                         , areaStreet  :: [OwnedTile]
                         }

market :: MarketId -> FieldOf Area Market
market mid = Field { .. }
  where
  getField a   = Map.findWithDefault emptyMarket mid (areaMarkets a)
  setField m a = a { areaMarkets = Map.insert mid m (areaMarkets a) }


emptyArea :: Int -> Area
emptyArea n = Area { areaMarkets = markets
                   , areaStreet  = [] }
  where
  markets = Map.fromList [ (MarketId i,emptyMarket) | i <- take n [ 0 .. ] ]


addTileArea :: MarketId -> OwnedTile -> Updater Area ()
addTileArea mid ot = with (market mid) (addTileMarket ot)


completeMarketArea :: MarketId -> Updater Area [OwnedTile]
completeMarketArea mid =
  do (win,loose) <- with (market mid) completeMarket
     upd $ \a -> a { areaStreet = loose ++ areaStreet a }
     pure win


-- XXX: use non-determinism?
autoPromoteArea :: [MarketId] -> OwnedTile -> Area -> [(MarketId,Area)]
autoPromoteArea excluded ot a =
  [ (mid,a1) | mid   <- Map.keys (areaMarkets a)
             , not (mid `elem` excluded)
             , Ok (_,a1) <- [ updater a (addTileArea mid ot) ]
             ]

rmVagrantArea :: OwnedTile -> Updater Area ()
rmVagrantArea ot =
  do street <- ask areaStreet
     case break (ot ==) street of
       (xs,_:ys) -> upd $ \a -> a { areaStreet = xs ++ ys }
       _ -> failure "Area does not have the required vagrant."


areaMoveSwap :: OwnedTile -> MarketId -> MarketId -> Updater Area Bool
areaMoveSwap ot from to =
  do mb <- with (market to) (marketMoveSwap ot)
     with (market from) $
       do rmTileMarket ot
          case mb of
            Nothing  -> pure ()
            Just pid -> addTileMarket ot { tileOwner = pid }
          ask marketIsFull

vagrantCanRejoin :: PlayerId -> Area -> Bool
vagrantCanRejoin pid a =
  any canRejoin [ tileType ot | ot <- areaStreet a, tileOwner ot == pid ]
  where
  canRejoin t = any (marketAccepts t) (areaMarkets a)



