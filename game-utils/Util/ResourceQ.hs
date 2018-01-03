{-# LANGUAGE Safe, RecordWildCards #-}
module Util.ResourceQ
  ( ResourceQ
  , rqEmpty
  , rqFromListRandom
  , rqFromListOrdered
  , rqTake
  , rqTakeMany
  , rqTakeUpTo
  , rqDiscard
  ) where

import Util.Random

data ResourceQ a = ResourceQ
  { qAvailable   :: [a]
  , qDiscarded   :: [a]
  , qRandom      :: StdGen
  }


rqEmpty :: Gen (ResourceQ a)
rqEmpty =
  do qRandom <- randStdGen
     return ResourceQ { qAvailable = [], qDiscarded = [], .. }

rqFromListRandom :: [a] -> Gen (ResourceQ a)
rqFromListRandom qDiscarded =
  do qRandom <- randStdGen
     return ResourceQ { qAvailable = [], .. }

rqFromListOrdered :: [a] -> Gen (ResourceQ a)
rqFromListOrdered qAvailable =
  do qRandom <- randStdGen
     return ResourceQ { qDiscarded = [], .. }

rqTake :: ResourceQ a -> Maybe (a, ResourceQ a)
rqTake ResourceQ { .. } =
  case qAvailable of
    a : as -> Just (a, ResourceQ { qAvailable = as, .. })
    [] -> case qDiscarded of
            [] -> Nothing
            _  -> Just $ let (a:as, g) = genRand qRandom (shuffle qDiscarded)
                         in (a, ResourceQ { qAvailable = as
                                          , qDiscarded = []
                                          , qRandom    = g })

rqTakeMany :: Int -> ResourceQ a -> Maybe ([a], ResourceQ a)
rqTakeMany n rq =
  case compare n 0 of
    GT -> do (a,rq1)  <- rqTake rq
             (as,rq2) <- rqTakeMany (n-1) rq1
             return (a : as, rq2)
    EQ -> return ([], rq)
    LT -> Nothing

rqTakeUpTo :: Int -> ResourceQ a -> ([a], ResourceQ a)
rqTakeUpTo n rq
  | n > 0, Just (a,rq1) <- rqTake rq = let (as,rq2) = rqTakeUpTo (n - 1) rq1
                                       in (a:as,rq2)
  | otherwise = ([], rq)


rqDiscard :: a -> ResourceQ a -> ResourceQ a
rqDiscard a ResourceQ { .. } = ResourceQ { qDiscarded = a : qDiscarded, .. }



