{-# Language TemplateHaskell #-}
module Basics where

import Control.Lens(makeLenses, (%~))

type PlayerId = Int

data Pawn = Pawn
  { _pawnPlayer :: !PlayerId
  , _pawnPower  :: !Int
  , _pawnSpeed  :: !Int
  } deriving Eq

$(makeLenses 'Pawn)

-- | A basic pawn for a player.
pawnNew :: PlayerId -> Pawn
pawnNew pid = Pawn { _pawnPlayer = pid
                   , _pawnPower  = 1
                   , _pawnSpeed  = 1
                   }

-- | Power levels.
nextPowerLevel :: Int -> Int
nextPowerLevel n
  | n == 1    = 2
  | n == 2    = 4
  | otherwise = n

-- | Speed levels.
nextSpeedLevel :: Int -> Int
nextSpeedLevel n
  | n == 1    = 2
  | n == 2    = 4
  | otherwise = n

pawnUpgradePower :: Pawn -> Pawn
pawnUpgradePower = pawnPower %~ nextPowerLevel

pawnUpgradeSpeed :: Pawn -> Pawn
pawnUpgradeSpeed = pawnSpeed %~ nextSpeedLevel


--------------------------------------------------------------------------------

data Token =
    PowerBoost
  | SpeedBoost
  | ShareSpace
    deriving (Eq,Ord)

data Tile =
    UpgradePower
  | UpgradeSpeed
  | GainPawn

  | GainToken Token

  | InvsetInPower
  | InverstInSpeed
  | InvestInSharing


