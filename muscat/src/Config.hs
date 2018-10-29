{-# Language OverloadedStrings #-}
{-# Language RecordWildCards #-}
module Config where

-- | How many types of tiles are there
tileNum :: Int
tileNum = 4

-- | How many of each tile do players start with
tileOfType :: Int
tileOfType = 4

-- | How many areas do we have to climb before the palace
areaNum :: Int
areaNum = 4

-- | How many tiles are promoted when a market is completed
winnerNum :: Int
winnerNum = ceiling (fromIntegral (tileNum - 1) / 2 :: Float)

-- | How many tiles do we need in the palace to end the game
promotedEnds :: Int -> Int
promotedEnds p = 4 + 2 * div p 2
