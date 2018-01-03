{-# LANGUAGE OverloadedStrings #-}
module Common where

import           Data.Text ( Text )
import           Text.PrettyPrint
import           Data.Char(toLower)


type PlayerId   = Int
type DeedName   = Text
type UnitName   = Text
type EnemyName  = Text


data Usable     = Unused | Used
                  deriving (Show,Eq)

data Visibility = Revealed | Hidden
                  deriving (Eq,Ord,Show)

data Element    = Physical | Fire | Ice | ColdFire
                  deriving (Eq,Ord,Show)

data Terrain    = Plains | Hills | Forest | Wasteland | Desert | Swamp
                | City BasicMana
                | Lake | Mountain
                | Ocean {- ^ for tile A and B -}
                  deriving (Eq,Ord,Show)


data AttackType = Melee | Ranged | Siege
                  deriving (Eq,Ord,Show)


data BasicMana  = Red | Green | Blue | White
                  deriving (Eq,Ord,Show)

data Mana       = BasicMana BasicMana | Gold | Black
                  deriving (Eq,Ord,Show)

data Time       = Day | Night
                  deriving (Eq,Ord,Show)



-- | Information about what sort of damage are we assigning.
data DamageInfo = DamageInfo { damageElement    :: Element
                             , damagePoisons    :: Bool
                             , damageParalyzes  :: Bool
                             }



anyBasicMana :: [ BasicMana ]
anyBasicMana = [ Red, Green, Blue, White ]

anyMana :: [ Mana ]
anyMana = Gold : Black : map BasicMana anyBasicMana

anyAttack :: [ AttackType ]
anyAttack = [ Melee, Ranged, Siege ]

anyElement :: [ Element ]
anyElement = [ Physical, Ice, Fire, ColdFire ]


ppElement :: Element -> Doc
ppElement el =
  case el of
    Physical  -> text "physical"
    Fire      -> text "fire"
    Ice       -> text "ice"
    ColdFire  -> text "cold fire"

ppBasicMana :: BasicMana -> Doc
ppBasicMana = text . map toLower . show

ppMana :: Mana -> Doc
ppMana m =
  case m of
    BasicMana b -> ppBasicMana b
    Gold        -> text "gold"
    Black       -> text "black"

ppTime :: Time -> Doc
ppTime t =
  case t of
    Day   -> text "day"
    Night -> text "night"


