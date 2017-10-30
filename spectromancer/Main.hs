{-# Language OverloadedStrings, RecordWildCards #-}
import Snap.Core(Snap,route)
import Snap.Http.Server (quickHttpServe)
import Snap.Util.FileServe(serveDirectory)
import Control.Applicative ((<|>))
import Control.Monad(unless)
import Control.Exception(try)
import Data.Aeson(toJSON, (.=))
import qualified Data.Aeson as JS
import Data.IORef(newIORef, readIORef, writeIORef)
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Map as Map

import Util.Snap(sendJSON, snapIO, badInput, notFound,
                    snapParam, snapParamSimpleEnum)
import Util.Random(randInt, randSourceIO, genRand)

import GameMonad
import Game
import ServerState
import CardTypes(cardsToJSON,Location(..))
import CardIds
import Cards(allCards)
import Turn
import Replay(Move(..))


sendGame :: GameId -> Game -> Log -> GameFinished -> Snap ()
sendGame gid g f w =
  do snapIO (mapM_ print (f []))
     sendJSON $ JS.object [ "game" .= g
                          , "log"  .= f []
                          , "gid"  .= gid
                          , "winner" .= (case w of
                                           NotFinished -> Nothing
                                           Winner who  -> Just who)
                          ]

sendError :: Text -> Log -> Snap ()
sendError t l = badInput t -- (Text.unwords (t : map (Text.pack . show) []))


snapGameM :: ServerState -> GameId -> Maybe Move -> Snap ()
snapGameM s gid mv =
  do let doThis = maybe (getGameById s gid) (makeMove s gid) mv
     mb <- snapIO (try doThis)
     case mb of
       Left err ->
         case err of
           GameNotFound -> notFound
           GameError err -> sendError err id
       Right (g,l,w) -> sendGame gid g l w


main :: IO ()
main =
  do s <- newServerState
     quickHttpServe $
          route
            [ ("getCards", snapGetCards)
            , ("getState", snapGetState s)
            , ("newGame",  snapNewGame s)
            , ("playCard", snapPlayCard s)
            , ("playTargetedCard", snapPlayTargetedCard s)
            , ("skipTurn", snapSkipTurn s)
            , ("listGames", snapListGames s)
            ]
           <|> serveDirectory "ui"


snapGetCards :: Snap ()
snapGetCards = sendJSON (cardsToJSON allCards)

snapGetState :: ServerState -> Snap ()
snapGetState s =
  do gid <- snapGameId
     snapIO $ print gid
     snapGameM s gid Nothing

snapNewGame :: ServerState -> Snap ()
snapNewGame self =
  do c1 <- snapCardClass "player1"
     c2 <- snapCardClass "player2"
     gen <- snapIO $ randSourceIO
     let (seed,_) = genRand gen randInt
     let gi = GameInit { rngSeed = seed
                       , firstPlayer = ("Player 1", c1)
                       , secondPlayer = ("Player 2", c2)
                       }

     (gid,game) <- snapIO $ addNewGame self gi
     sendGame gid game id NotFinished

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



