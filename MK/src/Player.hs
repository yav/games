{-# Language TemplateHaskell #-}
module Player where

import Control.Lens
import Util.ResourceQ
import Util.Bag

import Common
import Units
import Deed

data Skill = Skill
data Tactic = Tactic

data Player s = Player
  { _phase        :: s
    -- ^ State of the player that depends on the phase of the game

  , _units        :: [ActiveUnit]
  , _skills       :: [Skill]                -- ^ Aquired skills
  , _futureSkills :: ResourceQ Skill  -- ^ Skills that could be aquired later.

  , _fame         :: Int
  , _reputation   :: Int
  , _armor        :: Int
  , _handLimit    :: Int
  , _unitLimit    :: Int
  , _crystals     :: Bag BasicMana
  }

data BetweenRounds = BetweenRounds
  { _allCards     :: [Deed]
  }

data BetweenTurns = BetweenTurns
  { _tactic       :: Tactic
  , _deeds        :: ResourceQ Deed
  , _hand         :: [Deed]
  }

data DuringTurn = DuringTurn
  { _cards        :: BetweenTurns
  , _turnPhase    :: TurnPhase
  }

data TurnPhase = AtStartOfTurn      -- ^ Not chosen waht to do
               | DeclaredEndOfRound -- ^ Turn finished
               | RestingSlowly      -- ^ Turn finished
               | AtStartOfRest      -- ^ Wait to discard a non-wound card
               | Resting            -- ^ May discard wounds
               | Turn NormalTurn    -- ^ A normal turn


data NormalTurn = NormalTurn
  { _movement   :: Int
  , _attack     :: Bag (AttackType,Element)
  , _block      :: Bag Element
  , _influence  :: Int
  , _heal       :: Int

  , _mana         :: Bag Mana
  , _usedCrystals :: Bag BasicMana

  , _usedDiceReroll :: Bag Mana -- ^ Used mana dies that need rerolling
  , _usedDiceFixed  :: Bag Mana -- ^ Mana dice, no reroll

  , _playedCards :: [Deed] -- XXX: more strucutre

  , _normalTurnPhase :: NormalTurnPhase
  }

data NormalTurnPhase = Moving
                     | Exploring    -- ^ choose new tile location
                     | Interacting
                     | InCombat CombatPhase

data CombatPhase = CombatPhase


makeLenses ''Player
makeLenses ''BetweenRounds
makeLenses ''BetweenTurns
makeLenses ''DuringTurn
makeLenses 'NormalTurn


