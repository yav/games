{-# Language TemplateHaskell #-}
module Player where

import Control.Lens
import Data.Map(Map)
import Util.ResourceQ
import Util.Bag

import Terrain
import Common
import Units
import Skill(Skill)
import Tactic(Tactic)
import {-# SOURCE #-} Deed
import {-# SOURCE #-} Act

data Player = Player
  { _phase        :: TurnPhase
    -- ^ State of the player that depends on the phase of the game

  , location      :: Maybe Addr
    -- ^ Where we are on the board.  If "Nothing", then we arein the
    -- portal (i.e., not on the board).

  , _deeds        :: ResourceQ Deed
  , _hand         :: [Deed]

  , _tactic       :: Maybe Tactic

  , _units        :: [ActiveUnit]
  , _skills       :: [Skill]                -- ^ Aquired skills
  , _futureSkills :: ResourceQ Skill  -- ^ Skills that could be aquired later.

  , _fame         :: Int
  , _reputation   :: Int  -- ^ Reputatoin level (not effect!)
  , _armor        :: Int
  , _handLimit    :: Int
  , _unitLimit    :: Int
  , _crystals     :: Bag BasicMana
  }

data TurnPhase = BetweenRounds
               | BetweenTurns
               | AtStartOfTurn      -- ^ Not chosen what tto do
               | DeclaredEndOfRound -- ^ Turn finished
               | RestingSlowly      -- ^ Turn finished
               | AtStartOfRest      -- ^ Wait to discard a non-wound card
               | Resting            -- ^ May discard wounds
               | Turn NormalTurn    -- ^ A normal turn



data NormalTurn = NormalTurn
  { _movement   :: Int
  , _moveAmount :: Int    -- ^ Usually 1, but 2 with space bending.
  , _attack     :: Bag (AttackType,Element)
  , _block      :: Bag Element
  , _influence  :: Int
  , _heal       :: Int

  , _mana             :: Bag Mana
  , _usedCrystals     :: Bag BasicMana

  , _manaDice         :: Int      -- ^ How many mana dice can we use?
  , _usedDiceReroll   :: Bag Mana -- ^ Used mana dies that need rerolling
  , _usedDiceFixed    :: Bag Mana -- ^ Mana dice, no reroll

  , _playedCards      :: [Deed] -- XXX: more strucutre

  , _normalTurnPhase  :: NormalTurnPhase

  , _terrainCost      :: Map Terrain Int

  , _doAtEOT :: Act ()
    -- ^ Do this at the end of the turn
  }

data NormalTurnPhase = Moving
                     | Interacting
                     | InCombat CombatPhase

data CombatPhase = CombatRangedAttack
                 | CombatBlocking
                 | CombatDamage
                 | CombatAttack
                 | CombatFinished
                   deriving Eq


makeLenses ''Player
makeLenses 'NormalTurn



