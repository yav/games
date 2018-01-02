{-# Language OverloadedStrings, Rank2Types #-}
module Act where

import Control.Monad(liftM,ap)
import Control.Lens(Getter, view, to, (.~), (&), Lens')
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import Data.Set(Set)
import qualified Data.Set as Set
import MonadLib
import Util.Bag
import Util.Perhaps

import Common
import Player
import Terrain
import Enemies
import Game
import Land
import {-# SOURCE #-} Deed


newtype Act a = Act (StateT Game (ExceptionT Text IO) a)

instance Functor Act where
  fmap = liftM

instance Applicative Act where
  pure = Act . pure
  (<*>) = ap

instance Monad Act where
  Act m >>= k = Act (m >>= \a -> let Act m1 = k a in m1)


--------------------------------------------------------------------------------
-- Lens

class Rd t where
  rd :: Getter t a -> Act a

class Upd t where
  (?=) :: Lens' t a -> (a -> Perhaps a) -> Act ()

(%=) :: Upd t => Lens' t a -> (a -> a) -> Act ()
m %= f = updMb m (Right . f)

instance Rd Game where
  rd m = Act (view m <$> get)

instance Upd Game where
  m ?= f =
    do g <- Act get
       case f (view m g) of
         Failed err -> reportError err
         Ok a -> Act (set (g & m .~ a))

instance Rd Player where
  rd m = rd (curPlayer . m)

instance Upd Player where
  m ?= f = curPlayer . m %= f

instance Rd NormalTurn where
  rd m = do ph <- currentPhase
            case ph of
              Turn t -> return (view m t)
              _      -> reportError "Not in a normal turn."

instance Upd NormalTurn where
  m ?= f = phase ?= $ \a -> case a of
                              Turn t ->
                                do x <- f (view m t)
                                   return (Turn (t & m .~ x))
                              _ -> Failed "not in a normal turn."

instance Rd Land where
  rd m = rd (land . m)

instance Upd Land where
  updMb m f = updMb (land . m) f


-- Output

reportError :: Text -> Act a
reportError msg = Act (raise msg)


--------------------------------------------------------------------------------
-- Access to state

currentTime :: Act Time
currentTime = rd (to getTime)

currentPhase :: Act TurnPhase
currentPhase = rd phase

currentNormalTurnPhase :: Act NormalTurnPhase
currentNormalTurnPhase = rd normalTurnPhase

-- | Failes if not in combat
currentCombatPhase :: Act CombatPhase
currentCombatPhase =
  do ph <- currentNormalTurnPhase
     case ph of
       InCombat x -> return x
       _ -> reportError "Not in combat."

currentlyAvailableManaTypes :: Act [BasicMana]
currentlyAvailableManaTypes = rd (mana . to basic)
  where basic m = [ c  | BasicMana c <- Set.toList (bagKeys m) ]

currentCombatEnemyEffects :: Act (Set EnemyAbility)
currentCombatEnemyEffects = undefined

currentlyBlocking :: Act Enemy -- XXX: ActiveEnemy?
currentlyBlocking = undefined

atEOT :: Act () -> Act ()
atEOT a = doAtEOT %= (a >>)


--------------------------------------------------------------------------------

-- | Remove a die of the given color from the source
removeManaDie :: Mana -> Act ()
removeManaDie m = source %= \a -> fromMaybe a (bagRemove 1 m a)

--------------------------------------------------------------------------------



payMana :: Int -> Mana -> Act ()
payMana n m = updMb mana $ \x ->
              case bagRemove n m x of
                Nothing -> Left "Not enough mana."
                Just a -> Right a


looseReputation :: Int -> Act ()
looseReputation n = upd reputation (max (-7) . subtract n)

gainMana :: Int -> Mana -> Act ()
gainMana n m = upd mana (bagAdd n m)

gainManaDie :: Int -> Act ()
gainManaDie n = upd manaDice (+ n)

gainUsedManaDie :: Mana -> Act ()
gainUsedManaDie m = upd usedDiceReroll (bagAdd 1 m)

gainUsedManaDieFixed :: Mana -> Act ()
gainUsedManaDieFixed m = upd usedDiceFixed (bagAdd 1 m)

gainReputation :: Int -> Act ()
gainReputation n = upd reputation (min 7 . (+ n))

gainFame :: Int -> Act ()
gainFame n = upd fame (+ n)

gainMove :: Int -> Act ()
gainMove = undefined

gainCrystal :: Int -> BasicMana -> Act ()
gainCrystal = undefined

gainBlock :: Int -> Element -> Act ()
gainBlock = undefined

gainAttack :: Int -> AttackType -> Element -> Act ()
gainAttack = undefined

gainHeal :: Int -> Act ()
gainHeal = undefined

gainInfluence :: Int -> Act ()
gainInfluence = undefined

-- | Move from draw pile to hand.
-- If not enough cards, do as many as there are.
drawCard :: Int -> Act ()
drawCard = undefined

{- | Move a card from the hand to the played area.
If the card is not in hand, then it is not added to the play area.
This may happen when using a card from an offer trough a special
ability. -}
cardPlayed :: Deed -> Act ()
cardPlayed = undefined

-- | Discard the given card from your hand.
-- Fails if the card is not in hand.
discardCard :: Deed -> Act ()
discardCard = undefined

--------------------------------------------------------------------------------
-- Special modifiers

concentrated :: Int -> Act () -> Act ()
concentrated = undefined


--------------------------------------------------------------------------------
-- Input from the player

chooseSourceDie :: Act Mana
chooseSourceDie = undefined

chooseCardFromHand :: Act Deed
chooseCardFromHand = undefined

chooseCardFromDeeds :: Act Deed
chooseCardFromDeeds = undefined

chooseCardFromDiscard :: Act Deed
chooseCardFromDiscard = undefined

chooseCardFromSpellOffer :: Act Deed
chooseCardFromSpellOffer = undefined

chooseCardFromAdvancedAction :: Act Deed
chooseCardFromAdvancedAction = undefined

chooseAvailableMana :: Act BasicMana
chooseAvailableMana = undefined

chooseAvailableCrystal :: Act BasicMana
chooseAvailableCrystal = undefined

chooseManaFrom :: [Mana] -> Act Mana
chooseManaFrom = undefined

chooseBasicManaFrom :: [BasicMana] -> Act BasicMana
chooseBasicManaFrom ms =
  do BasicMana m <- chooseManaFrom (map BasicMana ms)
     return m

chooseAvailableUnit :: Act UnitName
chooseAvailableUnit = undefined

chooseEnemy :: Act EnemyName -- XXX: enemy id?
chooseEnemy = undefined

chooseLoc :: Act HexAddr
chooseLoc = undefined

askText :: Text -> [Text] -> Act Int
askText = undefined


