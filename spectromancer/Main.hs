{-# Language OverloadedStrings, RecordWildCards #-}
import Snap.Core(Snap,route,path)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory, serveFile)
import Control.Applicative ((<|>))
import Control.Monad(unless)
import Control.Exception(try)
import Data.Aeson(toJSON, (.=))
import qualified Data.Aeson as JS
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map

import Util.Snap(sendJSON, snapIO, badInput, notFound,
                    snapParam, snapParamSimpleEnum)
import Util.Random(randInt, randSourceIO, genRand)

import GameMonad
import Game
import ServerState
import CardTypes(Location(..),Who)
import Cards(allCards)
import Replay(Move(..),Replay(..))
import GameStats(winner)


sendGame :: GameId -> ActiveGame -> Snap ()
sendGame gid ag =
  case outcome r of
    GameOn g ->
      send [ "game"  .= g
           , "log"   .= (case states r of
                          [] -> []
                          (_,_,f) : _  -> f [])
           , "gid"    .= gid
           , "winner" .= (Nothing :: Maybe Who)
           ]

    GameStopped (GameWonBy w g) ->
      send [ "game"   .= g
           , "log"    .= ([] :: [LogEvent])
           , "gid"    .= gid
           , "winner" .= Just w
           ]

    GameStopped (Err err) -> sendError err id


  where
  r = replayLog ag

  send = sendJSON . JS.object . addGameLog

  addGameLog fs = ("history" .= [ JS.object [ "move" .= mv
                                            , "events" .= l []
                                            ] | (_, mv, l) <- reverse (states r) ])
                  : fs


sendError :: Text -> Log -> Snap ()
sendError t _l = badInput t -- (Text.unwords (t : map (Text.pack . show) []))


snapGameM :: ServerState -> GameId -> Maybe Move -> Snap ()
snapGameM s gid mv =
  do let doThis = maybe (getGameById s gid) (makeMove s gid) mv
     mb <- snapIO (try doThis)
     case mb of
       Left err ->
         case err of
           GameNotFound -> notFound
           GameError erro -> sendError erro id
       Right ag -> sendGame gid ag


main :: IO ()
main =
  do s <- newServerState
     quickHttpServe $
          route
            [ ("getState", snapGetState s)
            , ("newGame",  snapNewGame s)
            , ("playCard", snapPlayCard s)
            , ("playTargetedCard", snapPlayTargetedCard s)
            , ("skipTurn", snapSkipTurn s)
            , ("listGames", snapListGames s)
            ]
           <|> path "" (serveFile "ui/Play.html")
           <|> serveDirectory "ui"



snapGetState :: ServerState -> Snap ()
snapGetState s =
  do gid <- snapGameId
     snapIO $ print gid
     snapGameM s gid Nothing

snapNewGame :: ServerState -> Snap ()
snapNewGame self =
  do p1 <- snapParam "player1"
     p2 <- snapParam "player2"
     c1 <- snapCardClass "player1Class"
     c2 <- snapCardClass "player2Class"
     gen <- snapIO $ randSourceIO
     let (seed,_) = genRand gen randInt
     let gi = GameInit { rngSeed = seed
                       , firstPlayer = (p1, c1)
                       , secondPlayer = (p2, c2)
                       }

     (gid,ag) <- snapIO $ addNewGame self gi
     sendGame gid ag

snapListGames :: ServerState -> Snap ()
snapListGames self =
  do gms <- snapIO $ listGames self
     sendJSON $ toJSON gms

snapPlayCard :: ServerState -> Snap ()
snapPlayCard self =
  do g  <- snapGameId
     e  <- snapParamSimpleEnum "element"
     c  <- snapParam "card"
     snapGameM self g (Just (PlayCard e c Nothing))

snapSkipTurn :: ServerState -> Snap ()
snapSkipTurn self =
  do g <- snapGameId
     snapGameM self g (Just SkipTurn)

snapPlayTargetedCard :: ServerState -> Snap ()
snapPlayTargetedCard s =
  do g <- snapGameId
     e <- snapParamSimpleEnum "element"
     c <- snapParam "card"
     l <- snapParam "loc"
     w <- snapParamSimpleEnum "who"
     snapGameM s g
       (Just $ PlayCard e c (Just Location { locWho = w, locWhich = l }))

--------------------------------------------------------------------------------

snapCardClass :: Text -> Snap Text
snapCardClass pname =
  do e <- snapParam pname
     unless (e `Map.member` allCards) $
       sendError ("Invalid class in param: " `Text.append` pname) id
     return e

snapGameId :: Snap GameId
snapGameId = snapParam "gid"



