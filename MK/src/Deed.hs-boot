module Deed where

import {-# SOURCE #-} Act

data Deed
instance Eq Deed

deedBasic :: Deed -> Act ()
deedPower :: Deed -> Act ()
