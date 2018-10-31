{-# Language ExistentialQuantification #-}
module Interact where

-- import Data.ByteString(ByteString)
import Control.Monad(ap,liftM)
import Data.Text(Text)

import Updater
import Util.Perhaps

type Input = String

newtype Interact s a = I { unI :: (a -> s -> Msg s) -> s -> Msg s }

instance Functor (Interact s) where
  fmap = liftM

instance Applicative (Interact s) where
  pure a = I (\k -> k a)
  (<*>)  = ap

instance Monad (Interact s) where
  I m >>= f = I (\k -> m (\a -> unI (f a) k))

data Msg s = GetInput (Input -> Msg s)
           | Say Input (Msg s)
           | Done (Perhaps s)

getInput :: Interact s Input
getInput = I (\k s -> GetInput (\i -> k i s))

say :: Input -> Interact s ()
say x = I (\k s -> Say x (k () s))

update :: Updater s a -> Interact s a
update m = I (\k s -> case updater s m of
                        Ok (a,s1) -> k a s1
                        Failed err -> Done (Failed err))

-- label with no memory (i.e., captures the state as well)
label :: Interact s (Msg s)
label = I loopy
  where loopy k s = k (loopy k s) s

-- restart from the exact same point.
-- only difference would be the inputs.
jump :: Msg s -> Interact s a
jump m = I (\_ _ -> m)


newtype L s = L (s -> Msg s)

labelS = I loopy
  where loopy k s = k (L (loopy k)) s

jumpS :: L s -> Interact s a
jumpS (L l) = I (\_ s -> l s)


test :: Interact Int ()
test = do l <- labelS
          txt <- getInput
          update $ upd (+1)
          say.show =<< update get
          if txt == "q" then pure () else jumpS l

runI (I m) s = interpMsg $ m (\_ s -> Done (Ok s)) s


interpMsg :: Msg s -> IO (Perhaps s)
interpMsg msg =
  case msg of
    GetInput k -> do txt <- getLine
                     interpMsg (k txt)
    Say x k    -> do putStrLn x
                     interpMsg k
    Done a     -> pure a


