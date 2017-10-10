{-# Language OverloadedStrings #-}
module ServerState
  ( ServerState
  , newServerState
  , addNewGame
  , updateGame
  , GameException(..)
  , GameFinished(..)
  , GameId
  , listGames
  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.IORef(IORef,newIORef,atomicModifyIORef',readIORef)
import           Data.Text(Text)
import           Data.Time(UTCTime,NominalDiffTime,diffUTCTime,getCurrentTime)
import           Control.Exception(Exception(..), throwIO)
import           Control.Concurrent(threadDelay, forkIO)
import           Data.Monoid((<>))
import           Control.Lens((^.))

import Util.Random(StdGen, randSourceIO, genRandFun, randIdent)
import Game
import GameMonad
import CardTypes(Who)


type GameId = Text


data ServerState = ServerState (IORef PureState)

data PureState = PureState
  { activeGames   :: Map GameId ActiveGame
    -- ^ These are ongoing, not finished, games.

  , finishedGames :: Map GameId (Who,ActiveGame)
    -- ^ When a game finished, it is moved over here.

  , rngSeed       :: StdGen
  }

data ActiveGame = ActiveGame
  { activeGame    :: Game
  , lastActivity  :: UTCTime
  }


newServerState :: IO ServerState
newServerState =
  do seed <- randSourceIO
     ref  <- newIORef PureState { activeGames   = Map.empty
                                , finishedGames = Map.empty
                                , rngSeed       = seed }

     -- If there was no activity in a game for 15 minutes,
     -- we forget about it.
     let minutes = 60
     _ <- forkIO (gcThread (15 * minutes) ref)

     return (ServerState ref)

newActiveGame :: Game -> IO ActiveGame
newActiveGame game =
  do now <- getCurrentTime
     return ActiveGame { activeGame = game, lastActivity = now }

addNewGamePure :: ActiveGame -> PureState -> (PureState, GameId)
addNewGamePure ag ps =
  genRandFun (rngSeed ps) $
    do gid <- randIdent 64 (`Map.member` activeGames ps)
       return $ \newRng ->
         (  PureState { activeGames = Map.insert gid ag (activeGames ps)
                      , finishedGames = finishedGames ps
                      , rngSeed     = newRng }
         , gid
         )



addNewGame :: ServerState -> Game -> IO GameId
addNewGame (ServerState ref) game =
  do ag <- newActiveGame game
     atomicModifyIORef' ref (addNewGamePure ag)


listGames :: ServerState -> IO [(GameId, Text)]
listGames (ServerState s) =
  do st <- readIORef s
     return [(gid, pickName a) | (gid, a) <- Map.toList (activeGames st)]
  where pickName ag =
          let p1class = activeGame ag ^. curPlayer . playerClass
              p2class = activeGame ag ^. otherPlayer . playerClass
          in p1class <> " vs. " <> p2class

data GameFinished = NotFinished | Winner Who

-- | Perform the monadic computation in the given game context, and
-- return the new state of the corresponding game.
-- Throws 'GameException' if something goes wrong.
updateGame :: ServerState -> GameId -> GameM () -> IO (Game,Log,GameFinished)
updateGame (ServerState ref) gid m =
  do now <- getCurrentTime
     (res,f) <- atomicModifyIORef' ref (upd now)
     case res of
       Left err -> throwIO err
       Right (g,l) -> return (g,l,f)
  where
  upd now ps =
    case Map.lookup gid (activeGames ps) of
      Nothing -> (ps, (Left GameNotFound, NotFinished))
      Just ag ->
        case runGame (activeGame ag) m of

          -- Something went wrong, just bump the activite time stamp
          (GameStopped (Err err),_,_) ->
            let newAg = ag { lastActivity = now }
                newPs = ps { activeGames = Map.insert gid newAg (activeGames ps)
                           }
            in (newPs, (Left (GameError err), NotFinished))

          -- The game finished, add it to the finish list
          (GameStopped (GameWonBy w),g,l) ->
            let newAg = ActiveGame { activeGame = g, lastActivity = now }
                newPs = ps { finishedGames = Map.insert gid (w,newAg)
                                                (finishedGames ps)
                           , activeGames = Map.delete gid (activeGames ps)
                           }
            in (newPs, (Right (g, l), Winner w))

          -- We made some progress, jus update the active game
          (GameOn _, g, l) ->
            let newAg = ActiveGame { activeGame = g , lastActivity = now }
                newPs = ps { activeGames = Map.insert gid newAg (activeGames ps)
                           }
            in (newPs, (Right (g,l), NotFinished))



data GameException = GameNotFound
                   | GameError Text
                     deriving Show

instance Exception GameException


performGC :: UTCTime {- ^ Current time -} ->
             NominalDiffTime {- ^ Expireation time -} ->
             PureState {- ^ GC this -} ->
             (PureState, NominalDiffTime)
             -- ^ (how long to sleep for, new state)
performGC now okAge ps =
  let age ag  = diffUTCTime now (lastActivity ag)
      keep ag = age ag < okAge
      newPs = ps { activeGames = Map.filter keep (activeGames ps)
                 , finishedGames = Map.filter (keep . snd) (finishedGames ps)
                 }
      sleepTime x = okAge - age x
      sleepTimes  = map sleepTime (Map.elems (activeGames newPs)) ++
                    map (sleepTime . snd) (Map.elems (finishedGames newPs))

      largestST = minimum (okAge : sleepTimes)

  in (newPs, largestST)


gcThread :: NominalDiffTime -> IORef PureState -> IO a
gcThread age ref = go age
  where
  go sleepTime =
    do let x = fromEnum sleepTime `div` 10^(6::Int)
       threadDelay x
       now <- getCurrentTime
       print now
       go =<< atomicModifyIORef' ref (performGC now age)



