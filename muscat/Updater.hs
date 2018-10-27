module Updater where

import Data.Text(Text)
import Control.Monad(liftM,ap)
import Util.Perhaps

newtype Updater s a = U { unU :: s -> Perhaps (a,s) }

instance Functor (Updater s) where
  fmap = liftM

instance Applicative (Updater s) where
  pure a = U (\s -> Ok (a,s))
  (<*>)  = ap

instance Monad (Updater s) where
  U m >>= k = U (\s -> do (a,s1) <- m s
                          unU (k a) s1)

updater :: s -> Updater s a -> Perhaps (a,s)
updater s (U m) = m s

failure :: Text -> Updater s a
failure msg = U (\_ -> Failed msg)

get :: Updater s s
get = U (\s -> Ok (s,s))

set :: s -> Updater s ()
set s = U (\_ -> Ok ((),s))

upd :: (s -> s) -> Updater s ()
upd f = do s <- get
           set (f s)

ask :: (s -> a) -> Updater s a
ask f = do s <- get
           pure (f s)


data FieldOf r a = Field { getField :: r -> a
                         , setField :: a -> r -> r }

with :: FieldOf r f -> Updater f a -> Updater r a
with f m =
  do s <- get
     case unU m (getField f s) of
       Ok (a,si)  -> do set (setField f si s)
                        pure a
       Failed err -> failure err


