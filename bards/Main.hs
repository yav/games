{-# Language OverloadedStrings, RecordWildCards #-}
import Snap.Core(Snap,route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Control.Applicative ((<|>))
import Data.Aeson(toJSON)
import Data.IORef(newIORef, readIORef, atomicModifyIORef')
import Data.Text(Text)

import Util.Snap(sendJSON, snapIO, badInput, snapParam, snapParamSimpleEnum)
import Util.Perhaps(Perhaps(..))

import Game
import Basics
import Board
import Board(testLayout)
import JSON()

data ServerState = ServerState
  { readState :: Snap Game
  , modifyState :: (Game -> Perhaps Game) -> Snap Game
  }

newServerState :: Game -> IO ServerState
newServerState g0 =
  do s <- newIORef g0
     let readState = snapIO $ readIORef s
         modifyState f =
           do r <- snapIO $ atomicModifyIORef' s $ \g ->
                             let it = f g
                             in case f g of
                                  Failed err -> (g,it)
                                  Ok g1 -> (g1,it)
              case r of
                Failed err -> badInput err
                Ok g     -> return g
     return ServerState { .. }

main :: IO ()
main =
  do let Ok game = gameNew 2 testLayout

     s <- newServerState game

     quickHttpServe $
          route
            [ ("getState", snapGetState s)
            , ("newWorker", snapNewWorker s)
            , ("doAction", snapDoAction s)
            , ("move", snapMove s)
            , ("select", snapSelect s)
            ]
           <|> serveDirectory "ui"

snapJustModifyState :: ServerState -> (Game -> Perhaps Game) -> Snap ()
snapJustModifyState s f =
  do game <- modifyState s f
     sendJSON (toJSON game)

snapGetState :: ServerState -> Snap ()
snapGetState s =
  do game <- readState s
     sendJSON (toJSON game)

snapNewWorker :: ServerState -> Snap ()
snapNewWorker s = snapJustModifyState s . gamePlaceNewWorker =<< getLoc

snapDoAction :: ServerState -> Snap ()
snapDoAction s = snapJustModifyState s (Ok . gameDoAction)

snapMove :: ServerState -> Snap ()
snapMove s = snapJustModifyState s . gameMove =<< getLoc

snapSelect :: ServerState -> Snap ()
snapSelect s = snapJustModifyState s . gameSelectPawn =<< getLoc

getLoc :: Snap PawnLoc
getLoc =
  do x <- snapParam "x"
     y <- snapParam "y"
     return (PawnLoc x y)


