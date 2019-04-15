module BasicTypes where

data Resource       = Water | Fire | Life | Death | Gold
                      deriving (Eq,Ord)

data ComponentType  = Mage | MagicItem | Artifact | Monument | PlaceOfPower

data LifeForm       = Creature | Dragon

data Location       = Built | InHand | InDiscard | InDeck
