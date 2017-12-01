module EffectAPI where

import Control.Lens((^.))
import Data.Maybe(isNothing)

import Game
import Deck
import GameMonad
import CardTypes


data DamageSource = Attack Location   -- ^ Attack from creature in this slot
                  | Effect

data CreatureEffects = CreatureEffects
  {
    -- | Special ability at the start of turn.
    atStartOfTurn ::
      Location {- ^ Us -} ->
      GameM ()

    -- | Special ability at the end of turn.
  , atEndOfTurn ::
      Location {-^ Us -} ->
      GameM ()

    -- | Can this creature be summoned on the given location
  , validSummonLocation :: Game -> Location -> Bool

    -- | Modify the card being summoned.  This is mostly for the wolf,
    -- which needs to inherit the strength of the rabit.
    -- This happens at the very beginning of the summoning action
    -- (e.g., before the rabit is destroyed).
  , onSummonModification :: Game -> DeckCard -> DeckCard

    -- | We are being summoned.
  , onSummoned ::
      Location {-^ Our new location -} ->
      GameM ()

    -- | This happens just before an attack.  The result tells us if
    -- the creature should perform a standard attack after.  For creatures
    -- that have unusual attacks, the handler should return 'False',
    -- and the full attack should be implemented 
  , onBeforeAttack ::
      Location    {-^ Us -} ->
      GameM Bool  {-^ Returns if we should perform a standard attack or not -}

    -- | We succesfully damaged the opponent.
  , onDamageOpponent ::
      GameM ()

    -- | Someone is trying to deal damage to us.
  , onDamaged ::
      Location      {- ^ Us -} ->
      DamageSource  {- ^ How we got damaged -} ->
      Int           {-^ How much damage we are taking -} ->
      GameM Int

    -- | We died!
  , onDied ::
      Location {- ^ Where we used to be (not there anymore) -} ->
      GameM Bool  {- ^ 'True' if creature is really gone -}

    -- ^ We got destroyed!
  , onDestroyed ::
      Location {- ^ Where we used to be (not there anymore) -} ->
      GameM Bool {- ^ Should we go on and destroy the creature -}

    -- | We are being removed from the board, for whatever reason.
  , onRemoved ::
      Location {- ^ Where we used to be (not there anymore) -} ->
      DeckCard {- ^ This is us -} ->
      GameM ()


  -- Reactions

    -- | When another creature was summoned.
  , onSummonedOther ::
      Location {-^ Us -} ->
      Location {-^ Them -} ->
      GameM ()

    -- ^ When another creature died.
  , onDiedOther ::
      Location {-^ Us -} ->
      Location {-^ Them (now gone) -} ->
      GameM ()

    -- | How do we affect initial power growth of one of the players.
  , modifyPowerGrowth ::
      Who {- ^ Whose power is growing -} ->
      [(Element,Int)]

    -- | How we affect the cost of cards played by the opponent.
  , modifyCost ::
      DeckCard {- ^ Card the opponent wants to use -} ->
      Int

    -- | How we affect the attack of another creature (additive)
    -- NOTE THAT ARGUMENTS ARE BACKWARDS FROM EFFECTS
  , modifyAttack ::
      (Location,DeckCard) {- ^ Us -} ->
      (Location,DeckCard) {- ^ Them, attacking. -} ->
      Int

    -- | How we affect the amount of damage dealt to a creature (additive).
  , modifyCreatureDamageAdd ::
      Location            {-^ Us -} ->
      (Location,DeckCard) {-^ Who is being damaged -} ->
      Int

    -- | How we affect the amount of damage dealt to a creature (multiply).
  , modifyCreatureDamageMul ::
      Location            {-^ Us -} ->
      (Location,DeckCard) {-^ Who is being damaged -} ->
      Rational

    -- | Additive changes to a spell's damage.
  , modifySpellDamageAdd ::
      Who      {-^ Spell caster -} ->
      DeckCard {-^ This is us -} ->
      Int

    -- | Multiplicative changes to a spell's damage.
  , modifySpellDamageMul ::
      Who      {-^ Spell caster -} ->
      DeckCard {-^ This is us -} ->
      Rational

    -- | Do this whenever a player attempts to damage the opponent.
  , onOpponentAttacked ::
      Location    {- ^ us -} ->
      GameM Bool  {- ^ Should we go on and damage the opponent? -}

    -- | Additive changes to the wizard damage.
  , modifyWizardDamageAdd ::
      Who  {-^ Who is being damaged: Caster means "our summoner" -} ->
      Game {-^ Current state of the game -} ->
      Int

    -- | Multiplicative changes to the wizard damage.
  , modifyWizardDamageMul ::
      Who  {-^ Who is being damaged: Caster means "our summoner" -} ->
      Game {-^ Current state of the game -} ->
      Rational


  }


defaultCreature :: CreatureEffects
defaultCreature = CreatureEffects

  { atStartOfTurn           = \_ -> return ()
  , atEndOfTurn             = \_ -> return ()

  , validSummonLocation     = \g l -> and [ locWho l == Caster
                                          , locWhich l >= 0
                                          , locWhich l < slotNum
                                          , isNothing (g ^. creatureAt l)
                                          ]
  , onSummoned              = \_ -> return ()
  , onSummonModification    = \_ x -> x

  , onBeforeAttack          = \_ -> return True

  , onDamageOpponent        = return ()

  , onDamaged               = \l _ n -> creatureChangeLife l (negate n)
  , onDied                  = \_ -> return True

  , onDestroyed             = \_ -> return True

  , onRemoved               = \_ _ -> return ()


  , onSummonedOther         = \_ _ -> return ()
  , onDiedOther             = \_ _ -> return ()

  , modifyPowerGrowth       = \_ -> []
  , modifyCost              = \_ -> 0

  , modifyAttack            = \_ _ -> 0

  , modifyCreatureDamageAdd = \_ _ -> 0
  , modifyCreatureDamageMul = \_ _ -> 1

  , modifySpellDamageAdd    = \_ _ -> 0
  , modifySpellDamageMul    = \_ _ -> 1

  , onOpponentAttacked      = \_ -> return True
  , modifyWizardDamageAdd   = \_ _ -> 0
  , modifyWizardDamageMul   = \_ _ -> 1
  }



