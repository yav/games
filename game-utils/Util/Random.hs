{-# LANGUAGE Safe #-}
module Util.Random
  ( Gen
  , genRand, genRandFun
  , StdGen, randSource, randSourceIO

  , shuffle
  , oneOf
  , randInRange
  , randIdent
  , randStdGen
  ) where

import qualified System.Random as Rand
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Word(Word8)
import Data.Array (listArray, bounds, (!))
import Control.Monad(ap,liftM,replicateM)

newtype StdGen = StdGen Rand.StdGen

instance Show StdGen where
  show _ = "StdGen"

randSourceIO :: IO StdGen
randSourceIO = StdGen <$> Rand.newStdGen

randSource :: Int -> StdGen
randSource = StdGen . Rand.mkStdGen

genRand :: StdGen -> Gen a -> (a, StdGen)
genRand g m = genRandFun g $ do a <- m
                                return $ \r -> (a,r)

genRandFun :: StdGen -> Gen (StdGen -> a) -> a
genRandFun (StdGen g) (Gen m) = let (f,g1) = m g
                                in f (StdGen g1)


newtype Gen a = Gen (Rand.StdGen -> (a,Rand.StdGen))

instance Functor Gen where
  fmap = liftM

instance Applicative Gen where
  pure a = Gen (\g -> (a,g))
  (<*>)  = ap

instance Monad Gen where
  Gen m >>= k = Gen (\g -> let (a,g1) = m g
                               Gen m1 = k a
                           in m1 g1)


randStdGen :: Gen StdGen
randStdGen = Gen (\g -> let (g1,g2) = Rand.split g
                        in (StdGen g1, g2))

shuffle :: [a] -> Gen [a]
shuffle xs0 = Gen (\g0 -> go g0 (length xs0) xs0)
  where
  go g len xs | len < 2 = (xs, g)
  go g len xs = let (n,g1)    = Rand.randomR (0,len - 1) g
                    (as,b:bs) = splitAt n xs
                    (ys,g2)   = go g1 (len - 1) (as ++ bs)
                in (b : ys, g2)

oneOf :: [a] -> Gen a
oneOf xs = do let (l,h) = bounds as
              n <- randInRange l h
              return (as ! n)
  where
  as = listArray (0, length xs - 1) xs

randInRange :: Int -> Int -> Gen Int
randInRange l h = Gen (\g -> Rand.randomR (l,h) g)

-- | Keep running the random generator, until it generates
-- something that satisfies the predicate.  Note that this
-- process is not guaranteed to stop.
randomTill :: (a -> Bool) -> Gen a -> Gen a
randomTill p g =
  do a <- g
     if p a then return a else randomTill p g

-- | Generate a random byte
randByte :: Gen Word8
randByte = fromIntegral <$> randInRange 0 255

randIdent :: Int {- ^ How long -} ->
            (ByteString -> Bool) {- ^ Is it used -} ->
            Gen ByteString
randIdent n used = randomTill (not . used) (BS.pack <$> replicateM n randChar)
  where
  randChar = fromIntegral <$> randInRange 65 90

