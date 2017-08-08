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
import Board(testLayout)
import JSON()

data ServerState = ServerState
  { readState :: Snap Game
  , modifyState :: (Game -> Either Text Game) -> Snap Game
  }

newServerState :: Game -> IO ServerState
newServerState g0 =
  do s <- newIORef g0
     let readState = snapIO $ readIORef s
         modifyState f =
           do r <- snapIO $ atomicModifyIORef' s $ \g ->
                            case f g of
                              Left err -> (g,Left err)
                              Right g1 -> (g1, Right g1)
              case r of
                Left err -> badInput err
                Right g  -> return g
     return ServerState { .. }

main :: IO ()
main =
  do let Ok game = gameNew 2 testLayout

     s <- newServerState game

     quickHttpServe $
          route
            [ ("getState", snapGetState s)
            ]
           <|> serveDirectory "ui"


snapGetState :: ServerState -> Snap ()
snapGetState s =
  do game <- readState s
     sendJSON (toJSON game)

