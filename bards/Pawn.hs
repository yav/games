module Pawn where

type PlayerId = Int

data PawnId = PawnId
  { pidOwner   :: !PlayerId
  , pidNumber  :: !Int
  }
  deriving Eq

data Pawn = Pawn
  { pawnPower      :: !Int
  , pawnSpeed      :: !Int
  , pawnId         :: !PawnId
  , pawnIncognito  :: !Bool
  , pawnBoost      :: !Int
  } deriving Eq

-- | A basic pawn for a player.
pawnNew :: PawnId -> Pawn
pawnNew pid =
  Pawn
    { pawnId         = pid
    , pawnPower      = 1
    , pawnSpeed      = 1
    , pawnIncognito  = False
    , pawnBoost      = 0
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
pawnCurPower p = pawnPower p + pawnBoost p

pawnClear :: Pawn -> Pawn
pawnClear p = p { pawnBoost = 0, pawnIncognito = False }

