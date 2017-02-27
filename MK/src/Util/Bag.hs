{-# LANGUAGE Safe #-}
module Util.Bag
  ( Bag
  , bagEmpty, bagSingleton, bagAdd, bagUnion, bagMap

  , bagRemove, bagRemoveAll

  , bagLookup

  , bagIsEmpty, bagSize
  , bagKeys
  , bagToListGrouped, flatGrouped
  , bagToMap
  , bagToList
  , bagFromList
  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.Set ( Set )

-- Invariant: the amount of each resource is strictly positive.

newtype Bag a = Bag (Map a Int)
                deriving (Eq,Ord,Show)

bagEmpty :: Bag a
bagEmpty = Bag Map.empty

bagIsEmpty :: Bag a -> Bool
bagIsEmpty (Bag m) = Map.null m

bagAdd :: Ord a => Int -> a -> Bag a -> Bag a
bagAdd q r (Bag m)
  | q > 0     = Bag (Map.insertWith (+) r q m)
  | otherwise = Bag m

bagSingleton :: Ord a => a -> Bag a
bagSingleton a = bagAdd 1 a bagEmpty

bagUnion :: Ord a => Bag a -> Bag a -> Bag a
bagUnion (Bag a) (Bag b) = Bag (Map.unionWith (+) a b)

bagRemove :: Ord a => Int -> a -> Bag a -> Maybe (Bag a)
bagRemove q r b@(Bag m) =
  case compare avail q of
    GT -> Just (Bag (Map.adjust (subtract q) r m))
    EQ -> Just (Bag (Map.delete r m))
    LT -> Nothing
  where
  avail = bagLookup r b

bagRemoveAll :: Ord a => a -> Bag a -> Bag a
bagRemoveAll a (Bag m) = Bag (Map.delete a m)

bagLookup :: Ord a => a -> Bag a -> Int
bagLookup r (Bag m) = Map.findWithDefault 0 r m

bagToListGrouped :: Bag a -> [(a,Int)]
bagToListGrouped (Bag m) = Map.toList m

bagToMap :: Bag a -> Map a Int
bagToMap (Bag m) = m

bagToList :: Bag a -> [a]
bagToList = flatGrouped . bagToListGrouped

bagFromList :: Ord a => [a] -> Bag a
bagFromList = foldr (bagAdd 1) bagEmpty

bagKeys :: Bag a -> Set a
bagKeys (Bag m) = Map.keysSet m

flatGrouped :: [(a,Int)] -> [a]
flatGrouped xs = [ x | (a,n) <- xs, x <- replicate n a ]

bagMap :: Ord b => (a -> b) -> Bag a -> Bag b
bagMap f = bagFromList . map f . bagToList

-- | Number of elements in the bag.
-- Linear in the number of element types.
bagSize :: Bag a -> Int
bagSize = sum . map snd . bagToListGrouped

