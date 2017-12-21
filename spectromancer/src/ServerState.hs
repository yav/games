{-# Language OverloadedStrings #-}
module ServerState
  ( ServerState
  , newServerState
  , addNewGame
  , makeMove
  , getGameById
  , GameException(..)
  , GameId
  , listGames
  , ActiveGame(..)
  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Data.IORef(IORef,newIORef,atomicModifyIORef',readIORef)
import           Data.Text(Text)
import           Data.Time(UTCTime,NominalDiffTime,diffUTCTime,getCurrentTime)
import           Control.Exception(Exception(..), throwIO)
import           Control.Concurrent(threadDelay, forkIO)
import           Data.Monoid((<>))

import Util.Random(StdGen, randSourceIO, genRandFun, randIdent)
import Game hiding (rngSeed)
import GameMonad
import Replay(Move, Replay(..), emptyReplay, addReplayMove)
import Turn(GameInit)
import GameStats(finishGame)

type GameId = Text


data ServerState = ServerState (IORef PureState)

data PureState = PureState
  { activeGames   :: Map GameId ActiveGame
    -- ^ All games we know about.  Some might have finished.

  , rngSeed       :: StdGen
  }

data ActiveGame = ActiveGame
  { lastActivity  :: UTCTime
  , replayLog     :: Replay
  }


newServerState :: IO ServerState
newServerState =
  do seed <- randSourceIO
     ref  <- newIORef PureState { activeGames   = Map.empty
                                , rngSeed       = seed }

     -- If there was no activity in a game for 15 minutes,
     -- we forget about it.
     let minutes = 60
     _ <- forkIO (gcThread (15 * minutes) ref)

     return (ServerState ref)

newActiveGame :: GameInit -> IO ActiveGame
newActiveGame gameInit =
  do now <- getCurrentTime
     return ActiveGame { lastActivity = now
                       , replayLog = emptyReplay gameInit
                       }

addNewGamePure :: ActiveGame -> PureState -> (PureState, GameId)
addNewGamePure ag ps =
  genRandFun (rngSeed ps) $
    do gid <- randIdent 64 (`Map.member` activeGames ps)
       return $ \newRng ->
         (  PureState { activeGames = Map.insert gid ag (activeGames ps)
                      , rngSeed     = newRng }
         , gid
         )



addNewGame :: ServerState -> GameInit -> IO (GameId, ActiveGame)
addNewGame (ServerState ref) gi =
  do ag <- newActiveGame gi
     gid <- atomicModifyIORef' ref (addNewGamePure ag)
     return (gid, ag)


listGames :: ServerState -> IO [(GameId, Text)]
listGames (ServerState s) =
  do st <- readIORef s
     return [(gid, pickName a) | (gid, a) <- Map.toList (activeGames st)]
  where pickName ag =
          let ini = initialState (replayLog ag)
              mkLab (nm,cl) = nm <> " with " <> cl
          in mkLab (firstPlayer ini) <> " vs. " <> mkLab (secondPlayer ini)



getGameById :: ServerState -> GameId -> IO ActiveGame
getGameById (ServerState ref) g =
  do st <- readIORef ref
     case Map.lookup g (activeGames st) of
        Nothing -> throwIO GameNotFound
        Just g1 -> return g1

-- | Perform the given move.
-- Throws 'GameException' if something goes wrong.
makeMove :: ServerState -> GameId -> Move -> IO ActiveGame
makeMove (ServerState ref) gid mv =
  do now <- getCurrentTime
     res <- atomicModifyIORef' ref (upd now)
     case res of
       Left err -> throwIO err
       Right ag ->
         do maybeSaveWinner ag
            return ag
  where
  upd now ps =
    case Map.lookup gid (activeGames ps) of
      Nothing -> (ps, Left GameNotFound)
      Just ag ->
        let oldR    = replayLog ag
            newR    = addReplayMove mv oldR
            newAG r = ActiveGame { lastActivity = now, replayLog = r }

        in case outcome newR of

             GameStopped (Err err) ->
               let newPs = ps { activeGames =
                                  Map.insert gid (newAG oldR) (activeGames ps) }

               in (newPs, Left (GameError err))

             _ -> let ag1 = newAG newR
                      newPs = ps { activeGames = Map.insert gid ag1
                                                            (activeGames ps) }
                  in (newPs, Right ag1)



maybeSaveWinner :: ActiveGame -> IO ()
maybeSaveWinner ag =
  case outcome r of
    GameStopped (GameWonBy w g) ->
      let f = finishGame w (initialState r) g
      in appendFile "games.log" (show f ++ "\n")
    _ -> return ()
  where
  r = replayLog ag


data GameException = GameNotFound
                   | GameError Text
                     deriving Show

instance Exception GameException


--------------------------------------------------------------------------------
-- Garbage collect inactive games

performGC :: UTCTime {- ^ Current time -} ->
             NominalDiffTime {- ^ Expireation time -} ->
             PureState {- ^ GC this -} ->
             (PureState, NominalDiffTime)
             -- ^ (how long to sleep for, new state)
performGC now okAge ps =
  let age ag  = diffUTCTime now (lastActivity ag)
      keep ag = age ag < okAge
      newPs = ps { activeGames = Map.filter keep (activeGames ps)
                 }
      sleepTime x = okAge - age x
      sleepTimes  = map sleepTime (Map.elems (activeGames newPs))

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



