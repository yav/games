{-# Language OverloadedStrings, RecordWildCards #-}
module Engine where

import Util.Bag
import Util.Perhaps
import Util.ResourceQ

newtype PlayerId = PlayerId Int
  deriving (Eq,Ord)

newtype Profficiency = Profficiency Int
  deriving (Eq,Ord)

pMaxLevel :: Int
pMaxLevel = 3


--------------------------------------------------------------------------------
data Worker = Worker
  { wProf   :: !(Bag Profficiency)
  , wLimit  :: !Int
  , wOwner  :: !PlayerId
  }


wNew :: PlayerId -> Worker
wNew wOwner = Worker { wProf = bagEmpty, wLimit = 3, .. }

wAddProf :: Profficiency -> Worker -> Perhaps Worker
wAddProf p Worker { .. } =
  do checkThat (bagSize wProf     < wLimit) "Out of profficiency slots."
     checkThat (bagLookup p wProf < pMaxLevel) "Profficiency maxed out."
     return $! Worker { wProf = bagAdd 1 p wProf, .. }

wRmProf :: Profficiency -> Worker -> Perhaps Worker
wRmProf p Worker { .. } =
  do b1 <- perhaps "Not profficient" (bagRemove 1 p wProf)
     return $! Worker { wProf = b1, .. }

wChangeProf :: Profficiency -> Profficiency -> Worker -> Perhaps Worker
wChangeProf old new w = wAddProf new =<< wRmProf old w

wIncreaseLimit :: Worker -> Worker
wIncreaseLimit Worker { .. } = Worker { wLimit = wLimit + 1, .. }
--------------------------------------------------------------------------------

data Location = Location
  { lType     :: Profficiency
  , lContnet  :: ResourceQ Action
  , lNext     :: Opts
  , lCur      :: Either Opts Worker

  }

data Opts = Opts Action Action


-- | Is this location free.
lFree :: Location -> Bool
lFree Location { .. } = case lCur of
                          Left _  -> True
                          Right _ -> False

-- | Place a worker on a location.
-- Fails if the location is already occupied.
-- Returns the worker's profficiency, and the available actions.
lVisit :: Worker -> Location -> Perhaps (Int,Opts,Location)
lVisit w Location { .. } =
  do acts@(Opts a1 a2) <- case lCur of
              Left as -> return as
              Right _ -> fail "Location occupied."
     let l = Location { lContnet = rqDiscard a1 (rqDiscard a2 lContnet)
                      , lCur     = Right w
                      , ..
                      }
         p = bagLookup lType (wProf w)
     l `seq` p `seq` return (p, acts, l)

-- | Restock the location, and return the worker, if any.
lRenew :: Location -> (Maybe Worker, Location)
lRenew Location { .. } =
  case lCur of
    Left _ -> (Nothing, Location { .. })
    Right w ->
      let l = case rqTakeMany 2 lContnet of
                Just ([a1,a2],newCont) ->
                  Location { lCur = Left lNext
                           , lNext = Opts a1 a2
                           , lContnet = newCont
                           , .. }
                _ -> error "[bug] Unable to restock location."
      in l `seq` (Just w, l)


data Action = XXX

--------------------------------------------------------------------------------

data Player = Player
  { pWorkers :: [Worker]
  -- other resources
  }

data Game = Game
  { gPlayers    :: [Player]
  , gLocations  :: [Location]
  }




