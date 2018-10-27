module Ids where

newtype MarketId  = MarketId Int deriving (Eq,Ord)
newtype PlayerId  = PlayerId Int deriving (Eq,Ord)
newtype AreaId    = AreaId   Int deriving (Eq,Ord)
