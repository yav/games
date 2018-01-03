{-# Language OverloadedStrings, Rank2Types #-}
module Act where

import Control.Monad(liftM,ap)
import Control.Lens(Getter, (^.), to, (.~), (&), Lens')
import Data.Text(Text)
import Data.Maybe(fromMaybe)
import Data.Set(Set)
import qualified Data.Set as Set
import MonadLib
import Util.Bag
import Util.Perhaps
import Util.ResourceQ

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

infix 1 ?=
infix 1 %=
infix 1 .=

class Upd t where
  (?=) :: Lens' t a -> (a -> Perhaps a) -> Act ()

(%=) :: Upd t => Lens' t a -> (a -> a) -> Act ()
m %= f = m ?= Ok . f

(.=) :: Upd t => Lens' t a -> a -> Act ()
m .= x = m ?= Ok . const x

instance Rd Game where
  rd m = do x <- Act get
            return (x ^. m)

instance Upd Game where
  m ?= f =
    do g <- Act get
       case f (g ^. m) of
         Failed err -> reportError err
         Ok a -> Act (set (g & m .~ a))

instance Rd Player where
  rd m = rd (curPlayer . m)

instance Upd Player where
  m ?= f = curPlayer . m ?= f

instance Rd NormalTurn where
  rd m = do ph <- currentPhase
            case ph of
              Turn t -> return (t ^. m)
              _      -> reportError "Not in a normal turn."

instance Upd NormalTurn where
  m ?= f = phase ?= \a ->
           case a of
             Turn t ->
               do x <- f (t ^. m)
                  return (Turn (t & m .~ x))
             _ -> Failed "not in a normal turn."

instance Rd Land where
  rd m = rd (land . m)

instance Upd Land where
  m ?= f = land . m ?= f


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
payMana n m = mana ?= \x ->
              case bagRemove n m x of
                Nothing -> Failed "Not enough mana."
                Just a  -> Ok a


looseReputation :: Int -> Act ()
looseReputation n = reputation %= max (-7) . subtract n

gainMana :: Int -> Mana -> Act ()
gainMana n m = mana %= bagAdd n m

gainManaDie :: Int -> Act ()
gainManaDie n = manaDice %= (+ n)

gainUsedManaDie :: Mana -> Act ()
gainUsedManaDie m = usedDiceReroll %= bagAdd 1 m

gainUsedManaDieFixed :: Mana -> Act ()
gainUsedManaDieFixed m = usedDiceFixed %= bagAdd 1 m

gainReputation :: Int -> Act ()
gainReputation n = reputation %= min 7 . (+ n)

gainFame :: Int -> Act ()
gainFame n = fame %= (+ n)

gainMove :: Int -> Act ()
gainMove n = movement %= (+ n)

gainCrystal :: Int -> BasicMana -> Act ()
gainCrystal n c = crystals %= bagAdd n c

gainBlock :: Int -> Element -> Act ()
gainBlock n e = block %= bagAdd n e

gainAttack :: Int -> AttackType -> Element -> Act ()
gainAttack n t e = attack %= bagAdd n (t,e)

gainHeal :: Int -> Act ()
gainHeal n = heal %= (+ n)

gainInfluence :: Int -> Act ()
gainInfluence n = influence %= (+ n)

-- | Move from draw pile to hand.
-- If not enough cards, do as many as there are.
drawCard :: Int -> Act ()
drawCard n =
  do ds <- rd deeds
     let (as,ds1) = rqTakeUpTo n ds
     deeds .= ds1
     hand  %= (as ++)

{- | Move a card from the hand to the played area.
If the card is not in hand, then it is not added to the play area.
This may happen when using a card from an offer trough a special
ability. -}
cardPlayed :: Deed -> Act ()
cardPlayed d =
  do inHand <- rd hand
     case break (== d) inHand of
       (as,b:bs) -> do hand .= as ++ bs
                       playedCards %= (b :)
       _ -> return ()

-- | Discard the given card from your hand.
-- Fails if the card is not in hand.
discardCard :: Deed -> Act ()
discardCard d =
  do inHand <- rd hand
     case break (== d) inHand of
       (as,b:bs) -> do hand .= as ++ bs
                       deeds %= rqDiscard b
       _ -> reportError "This card is not in hand."

--------------------------------------------------------------------------------
-- Special modifiers

-- | If the action produces move, influence, block, or attack,
-- then gain some more.
concentrated :: Int -> Act () -> Act ()
concentrated n m =
  do oldMove <- rd movement
     oldInf  <- rd influence
     oldBl   <- rd block
     oldAtt  <- rd attack

     m

     newMove <- rd movement
     newInf  <- rd influence
     newBl   <- rd block
     newAtt  <- rd attack

     when (newMove > oldMove) (gainMove n)
     when (newInf  > oldInf)  (gainInfluence n)
     sequence_ [ atKey el newBl oldBl (gainBlock n) | el <- anyElement ]
     sequence_ [ atKey (t,el) newAtt oldAtt (uncurry (gainAttack n))
                                    | el <- anyElement, t  <- anyAttack ]
  where
  atKey k bN bO f = when (bagLookup k bN > bagLookup k bO) (f k)


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


