{-# Language TemplateHaskell #-}
module Pawn where

import Control.Lens(makeLenses, (^.), (.~), (&))

type PlayerId = Int

data PawnId = PawnId
  { pidOwner   :: !PlayerId
  , pidNumber  :: !Int
  }
  deriving Eq

data Pawn = Pawn
  { _pawnPower      :: !Int
  , _pawnSpeed      :: !Int
  , _pawnId         :: !PawnId
  , _pawnIncognito  :: !Bool
  , _pawnBoost      :: !Int
  } deriving Eq

$(makeLenses ''Pawn)

-- | A basic pawn for a player.
pawnNew :: PawnId -> Pawn
pawnNew pid =
  Pawn
    { _pawnId         = pid
    , _pawnPower      = 1
    , _pawnSpeed      = 1
    , _pawnIncognito  = False
    , _pawnBoost      = 0
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

pawnCurPower :: Pawn -> Int
pawnCurPower p = (p ^. pawnPower) + (p ^. pawnBoost)

pawnClear :: Pawn -> Pawn
pawnClear p = p & pawnBoost     .~ 0
                & pawnIncognito .~ False

