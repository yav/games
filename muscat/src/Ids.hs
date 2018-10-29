module Ids
  ( MarketId, marketId, marketIds
  , AreaId, areaId, areaIds, firstArea, nextArea
  , PlayerId, playerId, playerIds
  ) where

import Config
import Control.Monad(guard)



-- | Identifies a market in an area.
newtype MarketId  = MarketId Int deriving (Eq,Ord)

-- | Try to convert an integer to a market id, for a game with this many players
marketId :: Int {-^ Number of players -} -> Int -> Maybe MarketId
marketId pnum = upTo pnum MarketId

-- | All market ids in a game with this many players.
marketIds :: Int {-^Number of players -} -> [MarketId]
marketIds pnum = ids pnum MarketId



-- | Identifies a player.
newtype PlayerId  = PlayerId Int deriving (Eq,Ord)

-- | Try to convert an integer to a player id, in a game with the given
-- number of players.
playerId :: Int {-^Number of players-} -> Int -> Maybe PlayerId
playerId pnum = upTo pnum PlayerId

-- | All player ids in a game with the given number of players.
playerIds :: Int {-^Number of players-} -> [PlayerId]
playerIds pnum = ids pnum PlayerId



-- | Identifies an area in the game.
newtype AreaId    = AreaId Int deriving (Eq,Ord)

-- | The first valid area.
firstArea :: AreaId
firstArea = AreaId 0

-- | The next area, if any.
nextArea :: AreaId -> Maybe AreaId
nextArea (AreaId x)
  | y < areaNum = Just (AreaId y)
  | otherwise   = Nothing
  where y = x + 1

-- | try to convert an integer to an AreaId.
areaId :: Int -> Maybe AreaId
areaId = upTo areaNum AreaId

-- | All area ids in a game.
areaIds :: [AreaId]
areaIds = ids areaNum AreaId



--------------------------------------------------------------------------------

upTo :: Int -> (Int -> a) -> Int -> Maybe a
upTo n f x =
  do guard (0 <= x && x < n)
     pure (f x)

ids :: Int -> (Int -> a) -> [a]
ids n f = [ f i | i <- take n [ 0 .. ] ]
