module NewRule where

import Control.Monad(liftM,ap)
import Data.Text(Text)
import Util.Perhaps

import Common
-- import Player
import Terrain


data Act a = Act -- XXX

instance Functor Act where
  fmap = liftM

instance Applicative Act where
  pure = undefined
  (<*>) = ap

instance Monad Act where
  (>>=) = undefined

--------------------------------------------------------------------------------
-- Output

reportError :: Text -> Act a
reportError = undefined


--------------------------------------------------------------------------------
-- Access to state



--------------------------------------------------------------------------------
-- Input from the player

chooseSourceDie :: Act Mana
chooseSourceDie = undefined

chooseCardFromHand :: Act DeedName
chooseCardFromHand = undefined

chooseCardFromDeeds :: Act DeedName
chooseCardFromDeeds = undefined

chooseCardFromDiscard :: Act DeedName
chooseCardFromDiscard = undefined

chooseCardFromSpellOffer :: Act DeedName
chooseCardFromSpellOffer = undefined

chooseCardFromAdvancedAction :: Act DeedName
chooseCardFromAdvancedAction = undefined

chooseAvailableMana :: Act BasicMana
chooseAvailableMana = undefined

chooseAvailableCrystal :: Act BasicMana
chooseAvailableCrystal = undefined

chooseManaFrom :: [Mana] -> Act Mana
chooseManaFrom = undefined

chooseAvailableUnit :: Act UnitName
chooseAvailableUnit = undefined

chooseEnemy :: Act EnemyName -- XXX: enemy id?
chooseEnemy = undefined

chooseLoc :: Act HexAddr
chooseLoc = undefined

askText :: Text -> [Text] -> Act Int
askText = undefined


