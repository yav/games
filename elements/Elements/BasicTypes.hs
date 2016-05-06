{-# LANGUAGE OverloadedStrings #-}
module Elements.BasicTypes where

import Util.JSON

data Element  = Fire | Earth | Water | Air
                deriving (Eq,Ord,Show)

data Spirit   = Death | Element Element | Birth
                deriving (Eq,Ord,Show)

class Enumerate t where
  enumerate :: [t]

instance Enumerate Element where
  enumerate = [ Fire, Earth, Water, Air ]

instance Enumerate Spirit where
  enumerate = Death : Birth : map Element enumerate



