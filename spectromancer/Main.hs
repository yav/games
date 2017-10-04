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

import GameMonad
import Game
import ServerState
import CardTypes(cardsToJSON,Location(..))
import CardIds
import Cards(allCards)
import Turn


sendGame :: GameId -> Game -> Log -> Snap ()
sendGame gid g f = sendJSON $ JS.object [ "game" .= g
                                        , "log"  .= f []
                                        , "gid"  .= gid
                                        ]

sendError :: Text -> Log -> Snap ()
sendError t l = badInput (Text.unwords (t : map (Text.pack . show) (l [])))

snapGameM :: ServerState -> GameId -> GameM () -> Snap ()
snapGameM s gid m =
  do mb <- snapIO (try (updateGame s gid m))
     case mb of
       Left err ->
         case err of
           GameNotFound -> notFound
           GameError err -> sendError err id
       Right (g,l) -> sendGame gid g l


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
            ]
           <|> serveDirectory "ui"


snapGetCards :: Snap ()
snapGetCards = sendJSON (cardsToJSON allCards)

snapGetState :: ServerState -> Snap ()
snapGetState s =
  do gid <- snapGameId
     snapGameM s gid (return ())

snapNewGame :: ServerState -> Snap ()
snapNewGame self =
  do c1 <- snapCardClass "player1"
     c2 <- snapCardClass "player2"
     (gid,game) <- snapIO $
        do g   <- newGameIO ("Player 3", c1) ("Player 4", c2)
           gid <- addNewGame self g
           return (gid,g)
     sendGame gid game id

snapPlayCard :: ServerState -> Snap ()
snapPlayCard self =
  do g  <- snapGameId
     e  <- snapParamSimpleEnum "element"
     c  <- snapParam "card"
     snapGameM self g (turnPlayCard e c Nothing)

snapSkipTurn :: ServerState -> Snap ()
snapSkipTurn self =
  do g <- snapGameId
     snapGameM self g (turnSkip)

snapPlayTargetedCard :: ServerState -> Snap ()
snapPlayTargetedCard s =
  do g <- snapGameId
     e <- snapParamSimpleEnum "element"
     c <- snapParam "card"
     l <- snapParam "loc"
     w <- snapParamSimpleEnum "who"
     snapGameM s g
       (turnPlayCard e c (Just Location { locWho = w, locWhich = l }))

--------------------------------------------------------------------------------

snapCardClass :: Text -> Snap Text
snapCardClass pname =
  do e <- snapParam pname
     unless (e `Map.member` allCards) $
       sendError ("Invalid class in param: " `Text.append` pname) id
     return e

snapGameId :: Snap GameId
snapGameId = snapParam "gid"



