module EffectAPI where

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

    -- | We are being summoned.
  , onSummoned ::
      Location {-^ Our new location -} ->
      GameM ()

    -- | It is our turn to attack.
  , onAttack ::
      Location {-^ Us -} ->
      GameM ()

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
      GameM ()

    -- | We are being removed from the board, for whatever reason.
  , onRemoved ::
      Location {- ^ Where we used to be (not there anymore) -} ->
      DeckCard {- ^ This is us -} ->
      GameM ()


  -- Reactions

    -- | When another creature was summoned.
  , onSummonedOther ::
      Location {- ^ Us -} ->
      Location {-^ Them -} ->
      GameM ()

    -- ^ When another creature died.
  , onDiedOther ::
      Location {- ^ Us -} ->
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

    -- | How we affect the attack of another creature
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
  , onOpponnetAttacked ::
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

  , onSummoned              = \_ -> return ()

  , onAttack                = undefined -- XXX: "normal" attack

  , onDamageOpponent        = return ()

  , onDamaged               = \l _ n -> creatureChangeLife l (negate n)
  , onDied                  = \_ -> return True

  , onDestroyed             = \_ -> return ()

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

  , onOpponnetAttacked      = \_ -> return True
  , modifyWizardDamageAdd   = \_ _ -> 0
  , modifyWizardDamageMul   = \_ _ -> 1
  }



