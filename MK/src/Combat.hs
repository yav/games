{-# Language TemplateHaskell #-}
module Combat where

import Control.Lens

import Enemies

type EnemyId = Int

data ActiveEnemy = ActiveEnemy
  { _enemyId        :: EnemyId      -- ^ Used to identify an enemy
  , _enemyGroup     :: Maybe Int    -- ^ Group of enemies to which this belongs
                                    -- 'Nothing' indicates summoned
  , _enemyOrig      :: Enemy        -- ^ Enemy token, as is printed
  , _enemyCur       :: Enemy        -- ^ Enemy token with modification
  , _enemyAttacks   :: Bool
  }

data CombatAttacking = CombatAttacking
  { _targets    :: [ ActiveEnemy ]      -- ^ These can be attacked
  , _defeated   :: [ ActiveEnemy ]      -- ^ Killed
  , _undefeated :: [ ActiveEnemy ]      -- ^ Tried and failed
  , _battle     :: [ ActiveEnemy ]      -- ^ Currently fighting (or about to)
  , _inBattle   :: Bool                 -- ^ Are we currently in battle
  , _aIsRanged  :: Bool                 -- ^ Is this the ranged attack phase
  }

data CombatBlock = CombatBlock
  { _needBlocking :: [ ActiveEnemy ]    -- ^ These need blocking
  , _blocked      :: [ ActiveEnemy ]    -- ^ These were successfully blocked
  , _unblocked    :: [ ActiveEnemy ]    -- ^ Tried and failed to block
  , _blocking     :: Maybe ActiveEnemy  -- ^ Who we are blocking if any
  }

makeLenses 'ActiveEnemy
makeLenses 'CombatAttacking
makeLenses 'CombatBlock

